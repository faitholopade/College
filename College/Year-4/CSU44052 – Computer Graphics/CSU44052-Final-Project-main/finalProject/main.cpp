#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <stb/stb_image.h>
#include "character.h"
#include <render/shader.h>
#include <iostream>
#include <vector>
#include <cmath>
#include <chrono>
#include <cstdlib>
#include <ctime>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

static Character myCharacter;

// Window parameters
static GLFWwindow *window;
static int windowWidth = 1024;
static int windowHeight = 768;

// Camera parameters
static glm::vec3 cameraPos(0.0f, 0.0f, 5.0f);
static glm::vec3 cameraFront(0.0f, 0.0f, -1.0f);
static glm::vec3 cameraUp(0.0f, 1.0f, 0.0f);

static float yawAngle = -90.0f;
static float pitchAngle = 0.0f;

static float FoV = 45.0f;
static float nearPlane = 0.1f;
static float farPlane = 2000.0f;

// Timing
static float deltaTime = 0.0f;
static float lastFrame = 0.0f;
static int frameCount = 0;
static double lastTimeFPS = 0.0;

// Mouse control
static bool firstMouse = true;
static float lastX = 1024 / 2.0f;
static float lastY = 768 / 2.0f;
static float mouseSensitivity = 0.1f;
static float cameraSpeed = 50.0f;

//  button states camera vs. light control
static bool leftMouseDown = false;
static bool rightMouseDown = false;

// Skybox
static GLuint skyboxVAO;
static GLuint skyboxTex;
static GLuint skyboxProgramID;

// Starfield
static GLuint starVAO;
static GLuint starProgramID;
static GLint starMVP_ID;
static GLint starBaseColor_ID;

//  astronaut & asteroids
static GLuint astronautVAO;
static GLuint asteroidVAO_high;
static GLuint asteroidVAO_low;

//  emerald island
static GLuint emeraldVAO, emeraldVBO;
static GLuint emeraldTextureID;
static GLuint emeraldProgramID;
static GLint emeraldMVP_ID, emeraldTex_ID;

//  textured objects
static GLuint objectProgramID;
static GLint objectMVP_ID, objectModel_ID, objectView_ID, objectNormalMatrix_ID;
static GLint objectDiffuseTex_ID, objectNormalMap_ID, objectBaseColor_ID;
static GLint objectLightPos_ID, objectViewPos_ID;

// Light
static glm::vec3 lightPos(100.0f,100.0f,100.0f);

struct Asteroid {
    glm::vec3 pos;
    glm::vec3 rotAxis;
    float rotSpeed;
    float scale;
};
static std::vector<Asteroid> asteroids;

// Particle system for meteor shower
struct Particle {
    glm::vec3 position;
    glm::vec3 velocity;
    glm::vec4 color;
    float life;
};
static std::vector<Particle> particles;
static const unsigned int MAX_PARTICLES = 1000;
static GLuint particleVAO, particleVBO;
static GLuint particleProgramID;
static GLint particleMVP_ID, particleColor_ID;

// texture loading
GLuint loadTexture(const char* path) {
    GLuint texID;
    glGenTextures(1,&texID);
    glBindTexture(GL_TEXTURE_2D, texID);

    int w,h,channels;
    unsigned char* data = stbi_load(path,&w,&h,&channels,4);
    if(data) {
        glTexImage2D(GL_TEXTURE_2D,0,GL_RGBA,w,h,0,GL_RGBA,GL_UNSIGNED_BYTE,data);
        glGenerateMipmap(GL_TEXTURE_2D);
        stbi_image_free(data);
    }
    else {
        std::cerr << "Failed to load texture: " << path << std::endl;
    }

    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_S,GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_WRAP_T,GL_REPEAT);

    return texID;
}

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode);
static void cursor_callback(GLFWwindow *window, double xpos, double ypos);
static void mouse_button_callback(GLFWwindow* window, int button, int action, int mods);
static void framebuffer_size_callback(GLFWwindow* window, int width, int height);

GLuint loadCubemap(std::vector<std::string> faces);

// Vertex structure for astronaut & asteroids
struct Vertex {
    glm::vec3 pos;
    glm::vec2 uv;
    glm::vec3 normal;
    glm::vec3 tangent;
    glm::vec3 bitangent;
};

// For asteroid circle light/shadow
static GLuint circleVAO = 0, circleVBO = 0;
static GLuint circleProgramID = 0;
static GLint circleMVP_ID = -1;
static GLint circleColor_ID = -1;
static const int CIRCLE_SEGMENTS = 32;

void createCircleGeometry() {
    std::vector<glm::vec3> circleVerts;
    circleVerts.push_back(glm::vec3(0.0f, 0.0f, 0.0f)); // center
    for(int i=0; i<=CIRCLE_SEGMENTS; i++){
        float theta = 2.0f*M_PI*(float)i/(float)CIRCLE_SEGMENTS;
        float x = cos(theta);
        float z = sin(theta);
        circleVerts.push_back(glm::vec3(x, 0.0f, z));
    }
    glGenVertexArrays(1, &circleVAO);
    glGenBuffers(1, &circleVBO);
    glBindVertexArray(circleVAO);
    glBindBuffer(GL_ARRAY_BUFFER, circleVBO);
    glBufferData(GL_ARRAY_BUFFER, circleVerts.size() * sizeof(glm::vec3), circleVerts.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(glm::vec3),(void*)0);
    glBindVertexArray(0);
}

void drawCircleLight(const glm::mat4 &MVP, const glm::vec3 &color) {
    glUseProgram(circleProgramID);
    glUniformMatrix4fv(circleMVP_ID,1,GL_FALSE,glm::value_ptr(MVP));
    glUniform3fv(circleColor_ID,1,glm::value_ptr(color));
    glBindVertexArray(circleVAO);
    glDrawArrays(GL_TRIANGLE_FAN, 0, CIRCLE_SEGMENTS+2);
    glBindVertexArray(0);
}

void initializeParticles() {
    glGenVertexArrays(1, &particleVAO);
    glGenBuffers(1, &particleVBO);
    glBindVertexArray(particleVAO);
    glBindBuffer(GL_ARRAY_BUFFER, particleVBO);
    glBufferData(GL_ARRAY_BUFFER, MAX_PARTICLES * sizeof(Particle), NULL, GL_STREAM_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Particle), (void*)0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Particle), (void*)(offsetof(Particle, color)));
    glBindVertexArray(0);
}

void respawnParticle(Particle &particle) {
    float theta = ((rand() % 360) * M_PI) / 180.0f;
    float phi = ((rand() % 360) * M_PI) / 180.0f;
    float speed = 50.0f + ((rand() % 100) / 100.0f) * 50.0f;
    particle.position = glm::vec3(0.0f, 0.0f, 0.0f);
    particle.velocity = glm::vec3(speed * cos(theta) * sin(phi),
                                  speed * sin(theta) * sin(phi),
                                  speed * cos(phi));
    particle.color = glm::vec4(1.0f, 0.5f, 0.0f, 1.0f);
    particle.life = 5.0f;
}

// shadow map resolution
static const unsigned int SHADOW_WIDTH = 1024, SHADOW_HEIGHT = 1024;

// FBO & cubemap for depth
static GLuint depthMapFBO;
static GLuint depthCubemap;

//
static GLuint pointShadowProgram = 0;

// uniforms
static GLint shadowMatricesLoc = -1;
static GLint shadowLightPosLoc = -1;
static GLint shadowFarPlaneLoc = -1;
static GLint activeFaceIndexLoc = -1;

// views
std::vector<glm::mat4> makeShadowTransforms(const glm::vec3 &lightPos, float nearD, float farD) {
    glm::mat4 shadowProj = glm::perspective(glm::radians(90.0f), 1.0f, nearD, farD);
    std::vector<glm::mat4> matrices;
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3( 1, 0, 0), glm::vec3(0,-1, 0)));
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3(-1, 0, 0), glm::vec3(0,-1, 0)));
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3( 0, 1, 0), glm::vec3(0, 0, 1)));
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3( 0,-1, 0), glm::vec3(0, 0,-1)));
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3( 0, 0, 1), glm::vec3(0,-1, 0)));
    matrices.push_back(shadowProj * glm::lookAt(lightPos, lightPos + glm::vec3( 0, 0,-1), glm::vec3(0,-1, 0)));
    return matrices;
}

void renderDepthCubemap(const glm::vec3 &lightPos, float nearD, float farD) {
    glUseProgram(pointShadowProgram);
    std::vector<glm::mat4> shadowTransforms = makeShadowTransforms(lightPos, nearD, farD);
    for (int i = 0; i < 6; i++) {
        glUniformMatrix4fv(shadowMatricesLoc + i, 1, GL_FALSE, glm::value_ptr(shadowTransforms[i]));
    }
    glUniform3fv(shadowLightPosLoc, 1, glm::value_ptr(lightPos));
    glUniform1f(shadowFarPlaneLoc, farD);

    glViewport(0, 0, SHADOW_WIDTH, SHADOW_HEIGHT);
    glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
    glClear(GL_DEPTH_BUFFER_BIT);

    for(int face=0; face<6; face++) {
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
            GL_TEXTURE_CUBE_MAP_POSITIVE_X + face, depthCubemap, 0);
        glClear(GL_DEPTH_BUFFER_BIT);

        // face rendering to
        glUniform1i(activeFaceIndexLoc, face);

        glBindVertexArray(astronautVAO);
        glDrawArrays(GL_TRIANGLES,0,36);
        glBindVertexArray(0);

        for (auto &ast : asteroids) {
            glm::mat4 astModel(1.0f);
            astModel = glm::translate(astModel, ast.pos);
            astModel = glm::rotate(astModel, 0.0f , ast.rotAxis);
            astModel = glm::scale(astModel, glm::vec3(ast.scale));
            glBindVertexArray(asteroidVAO_high);
            glDrawArrays(GL_TRIANGLES,0,24);
            glBindVertexArray(0);
        }
    }
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // restore original viewport
    glViewport(0, 0, windowWidth, windowHeight);
}

static GLuint shortBoxVAO=0, shortBoxVBO=0, shortBoxEBO=0;
static GLuint tallBoxVAO=0, tallBoxVBO=0, tallBoxEBO=0;
static GLuint towerTex=0;

std::vector<Vertex> shortBoxVerts;
std::vector<unsigned int> shortBoxIndices;

std::vector<Vertex> tallBoxVerts;
std::vector<unsigned int> tallBoxIndices;

std::pair<glm::vec3,glm::vec3> computeTB(const glm::vec3 &p1,
                                         const glm::vec3 &p2,
                                         const glm::vec3 &p3,
                                         const glm::vec2 &uv1,
                                         const glm::vec2 &uv2,
                                         const glm::vec2 &uv3,
                                         const glm::vec3 &n) {
    glm::vec3 edge1 = p2 - p1;
    glm::vec3 edge2 = p3 - p1;
    glm::vec2 deltaUV1 = uv2 - uv1;
    glm::vec2 deltaUV2 = uv3 - uv1;
    float f = 1.0f/(deltaUV1.x*deltaUV2.y - deltaUV2.x*deltaUV1.y);

    glm::vec3 tangent, bitangent;

    tangent.x = f * (deltaUV2.y*edge1.x - deltaUV1.y*edge2.x);
    tangent.y = f * (deltaUV2.y*edge1.y - deltaUV1.y*edge2.y);
    tangent.z = f * (deltaUV2.y*edge1.z - deltaUV1.y*edge2.z);
    tangent = glm::normalize(tangent);

    bitangent.x = f * (-deltaUV2.x*edge1.x + deltaUV1.x*edge2.x);
    bitangent.y = f * (-deltaUV2.x*edge1.y + deltaUV1.y*edge2.y);
    bitangent.z = f * (-deltaUV2.x*edge1.z + deltaUV1.x*edge2.z);
    bitangent = glm::normalize(bitangent);

    tangent = glm::normalize(tangent - n*glm::dot(n,tangent));
    bitangent = glm::normalize(bitangent - n*glm::dot(n,bitangent));
    return std::make_pair(tangent, bitangent);
}

void addFace(std::vector<Vertex> &verts, std::vector<unsigned int> &inds,
             glm::vec3 p1, glm::vec3 p2, glm::vec3 p3, glm::vec3 p4,
             glm::vec2 uv1, glm::vec2 uv2, glm::vec2 uv3, glm::vec2 uv4) {
    glm::vec3 n = glm::normalize(glm::cross(p2-p1,p3-p1));
    auto tb1 = computeTB(p1,p2,p3, uv1,uv2,uv3,n);
    auto tb2 = computeTB(p1,p3,p4, uv1,uv3,uv4,n);

    Vertex v1,v2,v3,v4;
    v1.pos = p1; v1.uv=uv1; v1.normal=n; v1.tangent=tb1.first; v1.bitangent=tb1.second;
    v2.pos = p2; v2.uv=uv2; v2.normal=n; v2.tangent=tb1.first; v2.bitangent=tb1.second;
    v3.pos = p3; v3.uv=uv3; v3.normal=n; v3.tangent=tb1.first; v3.bitangent=tb1.second;
    v4.pos = p4; v4.uv=uv4; v4.normal=n; v4.tangent=tb2.first; v4.bitangent=tb2.second;

    unsigned int startIndex = (unsigned int)verts.size();
    verts.push_back(v1);
    verts.push_back(v2);
    verts.push_back(v3);
    verts.push_back(v4);

    inds.push_back(startIndex+0);
    inds.push_back(startIndex+1);
    inds.push_back(startIndex+2);
    inds.push_back(startIndex+0);
    inds.push_back(startIndex+2);
    inds.push_back(startIndex+3);
}

void buildShortBox() {
    shortBoxVerts.clear();
    shortBoxIndices.clear();

    glm::vec3 A(-130,165,-65);
    glm::vec3 B(-82,165,-225);
    glm::vec3 C(-240,165,-272);
    glm::vec3 D(-290,165,-114);
    glm::vec3 E(-130,0,-65);
    glm::vec3 F(-82,0,-225);
    glm::vec3 G(-240,0,-272);
    glm::vec3 H(-290,0,-114);

    glm::vec2 uv1(0,0);
    glm::vec2 uv2(1,0);
    glm::vec2 uv3(1,1);
    glm::vec2 uv4(0,1);

    addFace(shortBoxVerts,shortBoxIndices, A,B,C,D, uv1,uv2,uv3,uv4);
    addFace(shortBoxVerts,shortBoxIndices, E,A,D,H, uv1,uv2,uv3,uv4);
    addFace(shortBoxVerts,shortBoxIndices, F,B,A,E, uv1,uv2,uv3,uv4);
    addFace(shortBoxVerts,shortBoxIndices, G,C,B,F, uv1,uv2,uv3,uv4);
    addFace(shortBoxVerts,shortBoxIndices, H,D,C,G, uv1,uv2,uv3,uv4);

    glGenVertexArrays(1,&shortBoxVAO);
    glGenBuffers(1,&shortBoxVBO);
    glGenBuffers(1,&shortBoxEBO);

    glBindVertexArray(shortBoxVAO);
    glBindBuffer(GL_ARRAY_BUFFER, shortBoxVBO);
    glBufferData(GL_ARRAY_BUFFER, shortBoxVerts.size()*sizeof(Vertex), &shortBoxVerts[0], GL_STATIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, shortBoxEBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, shortBoxIndices.size()*sizeof(unsigned int),
                 &shortBoxIndices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,uv)));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,normal)));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,tangent)));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,bitangent)));
    glBindVertexArray(0);
}

void buildTallBox() {
    tallBoxVerts.clear();
    tallBoxIndices.clear();

    glm::vec3 A(-423,330,-247);
    glm::vec3 B(-265,330,-296);
    glm::vec3 C(-314,330,-456);
    glm::vec3 D(-472,330,-406);
    glm::vec3 E(-423,0,-247);
    glm::vec3 F(-265,0,-296);
    glm::vec3 G(-314,0,-456);
    glm::vec3 H(-472,0,-406);

    glm::vec2 uv1(0,0);
    glm::vec2 uv2(1,0);
    glm::vec2 uv3(1,1);
    glm::vec2 uv4(0,1);

    addFace(tallBoxVerts,tallBoxIndices, A,B,C,D, uv1,uv2,uv3,uv4);
    addFace(tallBoxVerts,tallBoxIndices, E,A,D,H, uv1,uv2,uv3,uv4);
    addFace(tallBoxVerts,tallBoxIndices, H,D,C,G, uv1,uv2,uv3,uv4);
    addFace(tallBoxVerts,tallBoxIndices, G,C,B,F, uv1,uv2,uv3,uv4);
    addFace(tallBoxVerts,tallBoxIndices, F,B,A,E, uv1,uv2,uv3,uv4);

    glGenVertexArrays(1,&tallBoxVAO);
    glGenBuffers(1,&tallBoxVBO);
    glGenBuffers(1,&tallBoxEBO);

    glBindVertexArray(tallBoxVAO);
    glBindBuffer(GL_ARRAY_BUFFER, tallBoxVBO);
    glBufferData(GL_ARRAY_BUFFER, tallBoxVerts.size()*sizeof(Vertex), &tallBoxVerts[0], GL_STATIC_DRAW);

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, tallBoxEBO);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, tallBoxIndices.size()*sizeof(unsigned int),
                 &tallBoxIndices[0], GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,uv)));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,normal)));
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,tangent)));
    glEnableVertexAttribArray(4);
    glVertexAttribPointer(4,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,bitangent)));
    glBindVertexArray(0);
}

int main(void) {
    if (!glfwInit()) {
        std::cerr << "Failed to initialise GLFW." << std::endl;
        return -1;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR,3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR,3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT,GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE,GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(windowWidth, windowHeight, "Toward a Futuristic Emerald Isle", NULL, NULL);
    if (window == NULL) {
        std::cerr << "Failed to open GLFW window." << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);

    glfwSetKeyCallback(window, key_callback);
    glfwSetCursorPosCallback(window, cursor_callback);
    glfwSetMouseButtonCallback(window, mouse_button_callback);
    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    // cursor for controlling camera with mouse and also for light
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_NORMAL);

    if (!gladLoadGL((GLADloadfunc)glfwGetProcAddress)) {
        std::cerr << "Failed to initialise GLAD." << std::endl;
        return -1;
    }

    glEnable(GL_DEPTH_TEST);

    // Skybox
    float skyboxVertices[] = {
        // positions
        -1.0f,  1.0f, -1.0f,
        -1.0f, -1.0f, -1.0f,
         1.0f, -1.0f, -1.0f,
         1.0f, -1.0f, -1.0f,
         1.0f,  1.0f, -1.0f,
        -1.0f,  1.0f, -1.0f,

        -1.0f, -1.0f,  1.0f,
        -1.0f, -1.0f, -1.0f,
        -1.0f,  1.0f, -1.0f,
        -1.0f,  1.0f, -1.0f,
        -1.0f,  1.0f,  1.0f,
        -1.0f, -1.0f,  1.0f,

         1.0f, -1.0f, -1.0f,
         1.0f, -1.0f,  1.0f,
         1.0f,  1.0f,  1.0f,
         1.0f,  1.0f,  1.0f,
         1.0f,  1.0f, -1.0f,
         1.0f, -1.0f, -1.0f,

        -1.0f, -1.0f,  1.0f,
        -1.0f,  1.0f,  1.0f,
         1.0f,  1.0f,  1.0f,
         1.0f,  1.0f,  1.0f,
         1.0f, -1.0f,  1.0f,
        -1.0f, -1.0f,  1.0f,

        -1.0f,  1.0f, -1.0f,
         1.0f,  1.0f, -1.0f,
         1.0f,  1.0f,  1.0f,
         1.0f,  1.0f,  1.0f,
        -1.0f,  1.0f,  1.0f,
        -1.0f,  1.0f, -1.0f,

        -1.0f, -1.0f, -1.0f,
        -1.0f, -1.0f,  1.0f,
         1.0f, -1.0f, -1.0f,
         1.0f, -1.0f, -1.0f,
        -1.0f, -1.0f,  1.0f,
         1.0f, -1.0f,  1.0f
    };

    std::vector<std::string> faces {
        "../finalProject/textures/skybox/right.jpg",
        "../finalProject/textures/skybox/left.jpg",
        "../finalProject/textures/skybox/top.jpg",
        "../finalProject/textures/skybox/bottom.jpg",
        "../finalProject/textures/skybox/front.jpg",
        "../finalProject/textures/skybox/back.jpg"
    };
    skyboxTex = loadCubemap(faces);

    {
        GLuint skyboxVBO;
        glGenVertexArrays(1, &skyboxVAO);
        glGenBuffers(1, &skyboxVBO);
        glBindVertexArray(skyboxVAO);
        glBindBuffer(GL_ARRAY_BUFFER, skyboxVBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(skyboxVertices), &skyboxVertices, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,3*sizeof(float),(void*)0);
        glBindVertexArray(0);
    }

    skyboxProgramID = LoadShadersFromFile("../finalProject/shader/skybox.vert", "../finalProject/shader/skybox.frag");

    // Starfield
    const int NUM_STARS = 10000;
    std::vector<glm::vec3> starPositions;
    starPositions.reserve(NUM_STARS);
    for(int i = 0; i < NUM_STARS; i++) {
        float x = ((rand() % 2000) - 1000.0f);
        float y = ((rand() % 2000) - 1000.0f);
        float z = ((rand() % 2000) - 1000.0f);
        starPositions.push_back(glm::vec3(x,y,z));
    }
    {
        GLuint starVBO;
        glGenVertexArrays(1, &starVAO);
        glGenBuffers(1, &starVBO);
        glBindVertexArray(starVAO);
        glBindBuffer(GL_ARRAY_BUFFER, starVBO);
        glBufferData(GL_ARRAY_BUFFER, starPositions.size() * sizeof(glm::vec3), &starPositions[0], GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,0,(void*)0);
        glBindVertexArray(0);
    }
    starProgramID = LoadShadersFromFile("../finalProject/shader/main.vert", "../finalProject/shader/main.frag");
    starMVP_ID = glGetUniformLocation(starProgramID, "MVP");
    starBaseColor_ID = glGetUniformLocation(starProgramID, "baseColor");

    // object shaders (astronaut & asteroids)
    objectProgramID = LoadShadersFromFile("../finalProject/shader/object_texture.vert","../finalProject/shader/object_texture.frag");
    objectMVP_ID = glGetUniformLocation(objectProgramID,"MVP");
    objectModel_ID = glGetUniformLocation(objectProgramID,"Model");
    objectView_ID = glGetUniformLocation(objectProgramID,"View");
    objectNormalMatrix_ID = glGetUniformLocation(objectProgramID,"NormalMatrix");
    objectDiffuseTex_ID = glGetUniformLocation(objectProgramID,"diffuseTex");
    objectNormalMap_ID = glGetUniformLocation(objectProgramID,"normalMap");
    objectBaseColor_ID = glGetUniformLocation(objectProgramID,"baseColor");
    objectLightPos_ID = glGetUniformLocation(objectProgramID,"lightPos");
    objectViewPos_ID = glGetUniformLocation(objectProgramID,"viewPos");

    // textures for astronaut/asteroids
    GLuint astronautDiffuse = loadTexture("../finalProject/textures/astronaut_diffuse.png");
    GLuint astronautNormal = loadTexture("../finalProject/textures/astronaut_normal.png");
    GLuint asteroidDiffuse = loadTexture("../finalProject/textures/asteroid_diffuse.png");
    GLuint asteroidNormal = loadTexture("../finalProject/textures/asteroid_normal.png");

    // astronaut geometry
    {
        glm::vec3 nFront(0,0,-1);
        glm::vec3 pF1(-0.5f,-0.5f,-0.5f), pF2(0.5f,-0.5f,-0.5f),
                  pF3(0.5f,0.5f,-0.5f), pF4(-0.5f,0.5f,-0.5f);
        glm::vec2 uvF1(0,1), uvF2(1,1), uvF3(1,0), uvF4(0,0);

        glm::vec3 nBack(0,0,1);
        glm::vec3 pB1(-0.5f,-0.5f,0.5f), pB2(0.5f,-0.5f,0.5f),
                  pB3(0.5f,0.5f,0.5f), pB4(-0.5f,0.5f,0.5f);
        glm::vec2 uvB1(1,1), uvB2(0,1), uvB3(0,0), uvB4(1,0);

        glm::vec3 nLeft(-1,0,0);
        glm::vec3 pL1(-0.5f,-0.5f,0.5f), pL2(-0.5f,-0.5f,-0.5f),
                  pL3(-0.5f,0.5f,-0.5f), pL4(-0.5f,0.5f,0.5f);
        glm::vec2 uvL1(0,1), uvL2(1,1), uvL3(1,0), uvL4(0,0);

        glm::vec3 nRight(1,0,0);
        glm::vec3 pR1(0.5f,-0.5f,-0.5f), pR2(0.5f,-0.5f,0.5f),
                  pR3(0.5f,0.5f,0.5f), pR4(0.5f,0.5f,-0.5f);
        glm::vec2 uvR1(0,1), uvR2(1,1), uvR3(1,0), uvR4(0,0);

        glm::vec3 nTop(0,1,0);
        glm::vec3 pT1(-0.5f,0.5f,-0.5f), pT2(0.5f,0.5f,-0.5f),
                  pT3(0.5f,0.5f,0.5f), pT4(-0.5f,0.5f,0.5f);
        glm::vec2 uvT1(0,1), uvT2(1,1), uvT3(1,0), uvT4(0,0);

        glm::vec3 nBot(0,-1,0);
        glm::vec3 pBo1(-0.5f,-0.5f,-0.5f), pBo2(0.5f,-0.5f,-0.5f),
                  pBo3(0.5f,-0.5f,0.5f), pBo4(-0.5f,-0.5f,0.5f);
        glm::vec2 uvBo1(0,0), uvBo2(1,0), uvBo3(1,1), uvBo4(0,1);

        auto computeTangentSpaceForCube = [&](glm::vec3 p1, glm::vec3 p2, glm::vec3 p3, glm::vec3 p4,
                                              glm::vec2 u1, glm::vec2 u2, glm::vec2 u3, glm::vec2 u4,
                                              glm::vec3 n, std::vector<Vertex>& verts){
            auto tb1 = computeTB(p1,p2,p3, u1,u2,u3,n);
            auto tb2 = computeTB(p1,p3,p4, u1,u3,u4,n);
            glm::vec3 t = tb1.first;
            glm::vec3 b = tb1.second;

            // triangles
            verts.push_back({p1,u1,n,t,b});
            verts.push_back({p2,u2,n,t,b});
            verts.push_back({p3,u3,n,t,b});
            verts.push_back({p1,u1,n,t,b});
            verts.push_back({p3,u3,n,t,b});
            verts.push_back({p4,u4,n,t,b});
        };

        std::vector<Vertex> astronautVertices;
        computeTangentSpaceForCube(pF1,pF2,pF3,pF4,uvF1,uvF2,uvF3,uvF4,nFront,astronautVertices);
        computeTangentSpaceForCube(pB1,pB2,pB3,pB4,uvB1,uvB2,uvB3,uvB4,nBack,astronautVertices);
        computeTangentSpaceForCube(pL1,pL2,pL3,pL4,uvL1,uvL2,uvL3,uvL4,nLeft,astronautVertices);
        computeTangentSpaceForCube(pR1,pR2,pR3,pR4,uvR1,uvR2,uvR3,uvR4,nRight,astronautVertices);
        computeTangentSpaceForCube(pT1,pT2,pT3,pT4,uvT1,uvT2,uvT3,uvT4,nTop,astronautVertices);
        computeTangentSpaceForCube(pBo1,pBo2,pBo3,pBo4,uvBo1,uvBo2,uvBo3,uvBo4,nBot,astronautVertices);

        GLuint astronautVBO;
        glGenVertexArrays(1, &astronautVAO);
        glGenBuffers(1, &astronautVBO);
        glBindVertexArray(astronautVAO);
        glBindBuffer(GL_ARRAY_BUFFER, astronautVBO);
        glBufferData(GL_ARRAY_BUFFER, astronautVertices.size()*sizeof(Vertex), &astronautVertices[0], GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,uv)));
        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,normal)));
        glEnableVertexAttribArray(3);
        glVertexAttribPointer(3,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,tangent)));
        glEnableVertexAttribArray(4);
        glVertexAttribPointer(4,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,bitangent)));
        glBindVertexArray(0);
    }

    // high detail asteroid
    std::vector<glm::vec3> astVerts_high = {
        glm::vec3(0,1,0), glm::vec3(-1,0,0), glm::vec3(0,0,1),
        glm::vec3(0,1,0), glm::vec3(0,0,1), glm::vec3(1,0,0),
        glm::vec3(0,1,0), glm::vec3(1,0,0), glm::vec3(0,0,-1),
        glm::vec3(0,1,0), glm::vec3(0,0,-1), glm::vec3(-1,0,0),

        glm::vec3(0,-1,0), glm::vec3(0,0,1), glm::vec3(-1,0,0),
        glm::vec3(0,-1,0), glm::vec3(1,0,0), glm::vec3(0,0,1),
        glm::vec3(0,-1,0), glm::vec3(0,0,-1), glm::vec3(1,0,0),
        glm::vec3(0,-1,0), glm::vec3(-1,0,0), glm::vec3(0,0,-1)
    };
    for (auto &p : astVerts_high) {
        p = glm::normalize(p)*(1.0f+((rand()%100)/1000.0f));
    }
    std::vector<Vertex> asteroidMesh_high;
    asteroidMesh_high.reserve(astVerts_high.size());
    for (auto &p : astVerts_high) {
        Vertex v;
        v.pos = p;
        v.normal = glm::normalize(p);
        float u = atan2(p.z,p.x)/(2*M_PI)+0.5f;
        float vtex = p.y*0.5f+0.5f;
        v.uv = glm::vec2(u,vtex);
        glm::vec3 dummyT(-p.z,0,p.x);
        dummyT = glm::normalize(dummyT);
        glm::vec3 dummyB = glm::normalize(glm::cross(v.normal,dummyT));
        v.tangent = dummyT;
        v.bitangent = dummyB;
        asteroidMesh_high.push_back(v);
    }
    {
        GLuint asteroidVBO_high;
        glGenVertexArrays(1, &asteroidVAO_high);
        glGenBuffers(1, &asteroidVBO_high);
        glBindVertexArray(asteroidVAO_high);
        glBindBuffer(GL_ARRAY_BUFFER, asteroidVBO_high);
        glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex)*asteroidMesh_high.size(),
                     &asteroidMesh_high[0], GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,uv)));
        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,normal)));
        glEnableVertexAttribArray(3);
        glVertexAttribPointer(3,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,tangent)));
        glEnableVertexAttribArray(4);
        glVertexAttribPointer(4,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,bitangent)));
        glBindVertexArray(0);
    }

    // low detail asteroid
    std::vector<glm::vec3> astVerts_low = {
        glm::vec3(0,1,0), glm::vec3(-1,0,0), glm::vec3(1,0,0),
        glm::vec3(0,1,0), glm::vec3(1,0,0),  glm::vec3(0,0,1),
        glm::vec3(0,1,0), glm::vec3(0,0,1),  glm::vec3(-1,0,0),
        glm::vec3(0,-1,0), glm::vec3(1,0,0), glm::vec3(-1,0,0),
        glm::vec3(0,-1,0), glm::vec3(0,0,1), glm::vec3(1,0,0)
    };
    for (auto &p : astVerts_low) {
        p = glm::normalize(p)*(1.0f+(rand()%20)/200.0f);
    }
    std::vector<Vertex> asteroidMesh_low;
    asteroidMesh_low.reserve(astVerts_low.size());
    for (auto &p : astVerts_low) {
        Vertex v;
        v.pos = p;
        v.normal = glm::normalize(p);
        float u = atan2(p.z,p.x)/(2*M_PI)+0.5f;
        float vtex = p.y*0.5f+0.5f;
        v.uv = glm::vec2(u,vtex);
        glm::vec3 dummyT(-p.z,0,p.x);
        dummyT = glm::normalize(dummyT);
        glm::vec3 dummyB = glm::normalize(glm::cross(v.normal,dummyT));
        v.tangent = dummyT;
        v.bitangent = dummyB;
        asteroidMesh_low.push_back(v);
    }
    {
        GLuint asteroidVBO_low;
        glGenVertexArrays(1, &asteroidVAO_low);
        glGenBuffers(1, &asteroidVBO_low);
        glBindVertexArray(asteroidVAO_low);
        glBindBuffer(GL_ARRAY_BUFFER, asteroidVBO_low);
        glBufferData(GL_ARRAY_BUFFER, sizeof(Vertex)*asteroidMesh_low.size(),
                     &asteroidMesh_low[0], GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,uv)));
        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,normal)));
        glEnableVertexAttribArray(3);
        glVertexAttribPointer(3,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,tangent)));
        glEnableVertexAttribArray(4);
        glVertexAttribPointer(4,3,GL_FLOAT,GL_FALSE,sizeof(Vertex),(void*)(offsetof(Vertex,bitangent)));
        glBindVertexArray(0);
    }

    srand((unsigned int)time(nullptr));
    for (int i=0; i<100; i++){
        Asteroid a;
        a.pos = glm::vec3((rand()%400)-200,(rand()%200)-100,(rand()%400)-200);
        a.rotAxis = glm::normalize(glm::vec3((rand()%100)-50,(rand()%100)-50,(rand()%100)-50));
        a.rotSpeed = (rand()%100)/50.0f;
        a.scale = (rand()%10 + 5);
        asteroids.push_back(a);
    }

    // Emerald islands
    float emeraldVertices[] = {
        // position         // uv
        -5.0f,-0.5f,-5.0f, 0.0f,0.0f,
         5.0f,-0.5f,-5.0f, 1.0f,0.0f,
         5.0f,-0.5f, 5.0f, 1.0f,1.0f,
         5.0f,-0.5f, 5.0f, 1.0f,1.0f,
        -5.0f,-0.5f, 5.0f, 0.0f,1.0f,
        -5.0f,-0.5f,-5.0f, 0.0f,0.0f
    };
    {
        glGenVertexArrays(1, &emeraldVAO);
        glGenBuffers(1, &emeraldVBO);
        glBindVertexArray(emeraldVAO);
        glBindBuffer(GL_ARRAY_BUFFER, emeraldVBO);
        glBufferData(GL_ARRAY_BUFFER, sizeof(emeraldVertices), emeraldVertices, GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0,3,GL_FLOAT,GL_FALSE,5*sizeof(float),(void*)0);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1,2,GL_FLOAT,GL_FALSE,5*sizeof(float),(void*)(3*sizeof(float)));
        glBindVertexArray(0);
    }
    emeraldProgramID = LoadShadersFromFile("../finalProject/shader/emerald.vert","../finalProject/shader/emerald.frag");
    emeraldMVP_ID = glGetUniformLocation(emeraldProgramID,"MVP");
    emeraldTex_ID = glGetUniformLocation(emeraldProgramID,"emeraldTexture");
    emeraldTextureID = loadTexture("../finalProject/textures/emerald.png");

    lastTimeFPS = glfwGetTime();

    float asteroidAngle = 0.0f;
    float astronautScale = 10.0f;

    // Character from Lab 4
    if(!myCharacter.initialize("../finalProject/model/bot/bot.gltf")) {
        std::cerr << "Failed to load character model.\n";
    }

    // Circle geom for asteroid's light + shadow
    createCircleGeometry();
    circleProgramID = LoadShadersFromFile("../finalProject/shader/circle_light.vert","../finalProject/shader/circle_light.frag");
    circleMVP_ID   = glGetUniformLocation(circleProgramID, "MVP");
    circleColor_ID = glGetUniformLocation(circleProgramID, "circleColor");

    // init particle system
    initializeParticles();
    particleProgramID = LoadShadersFromFile("../finalProject/shader/particle.vert", "../finalProject/shader/particle.frag");
    if(!particleProgramID) {
        std::cerr << "Failed to load particle shaders.\n";
    }
    particleMVP_ID = glGetUniformLocation(particleProgramID, "MVP");
    particleColor_ID = glGetUniformLocation(particleProgramID, "particleColor");

    // particles
    particles.resize(MAX_PARTICLES);
    for(auto &p : particles) {
        respawnParticle(p);
    }

    glGenFramebuffers(1, &depthMapFBO);
    glGenTextures(1, &depthCubemap);
    glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);
    for (unsigned int i = 0; i < 6; i++){
        glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0, GL_DEPTH_COMPONENT,
                     SHADOW_WIDTH, SHADOW_HEIGHT, 0, GL_DEPTH_COMPONENT, GL_FLOAT, NULL);
    }
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_R,GL_CLAMP_TO_EDGE);

    glBindFramebuffer(GL_FRAMEBUFFER, depthMapFBO);
    glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, depthCubemap, 0);
    glDrawBuffer(GL_NONE);
    glReadBuffer(GL_NONE);
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // point shadow depth pass shaders
    pointShadowProgram = LoadShadersFromFile("../finalProject/shader/point_shadow.vert","../finalProject/shader/point_shadow.frag");
    if(!pointShadowProgram) {
        std::cerr << "Failed to load point shadow shaders.\n";
    }

    // uniform locations
    glUseProgram(pointShadowProgram);

    shadowMatricesLoc = glGetUniformLocation(pointShadowProgram, "shadowMatrices[0]");
    shadowLightPosLoc = glGetUniformLocation(pointShadowProgram, "lightPos");
    shadowFarPlaneLoc = glGetUniformLocation(pointShadowProgram, "far_plane");
    activeFaceIndexLoc = glGetUniformLocation(pointShadowProgram, "activeFaceIndex");

    glUseProgram(objectProgramID);
    GLint depthMapLoc = glGetUniformLocation(objectProgramID, "depthCubemap");
    glUniform1i(depthMapLoc, 2); // bind depth cubemap to texture unit 2
    GLint farPlaneLoc = glGetUniformLocation(objectProgramID, "far_plane");
    glUniform1f(farPlaneLoc, 1000.0f); // or whichever far used

    GLuint charProg = myCharacter.getProgramID();
    glUseProgram(charProg);
    GLint charDepthMapLoc = glGetUniformLocation(charProg, "depthCubemap");
    glUniform1i(charDepthMapLoc, 2);
    GLint charFarPlaneLoc = glGetUniformLocation(charProg, "far_plane");
    glUniform1f(charFarPlaneLoc, 1000.0f);

    buildShortBox();
    buildTallBox();
    towerTex = loadTexture("../finalProject/textures/tower.png");

    // main loop
    while(!glfwWindowShouldClose(window)) {
        double currentTime = glfwGetTime();
        deltaTime = (float)(currentTime - lastFrame);
        lastFrame = currentTime;

        static float animationTime = 0.0f;
        animationTime += deltaTime;
        myCharacter.update(animationTime, true, 2.0f);

        // FPS tracking
        frameCount++;
        if(currentTime - lastTimeFPS >= 1.0) {
            double fps = (double)frameCount / (currentTime - lastTimeFPS);
            frameCount = 0;
            lastTimeFPS = currentTime;
            std::string title = "Toward a Futuristic Emerald Isle - FPS: " + std::to_string((int)fps);
            glfwSetWindowTitle(window, title.c_str());
        }

        // Input
        glfwPollEvents();

        // Movement
        float currentSpeed = cameraSpeed * deltaTime;
        if(glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS)
            cameraPos += currentSpeed * cameraFront;
        if(glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)
            cameraPos -= currentSpeed * cameraFront;

        glm::vec3 cameraRight = glm::normalize(glm::cross(cameraFront, cameraUp));
        if(glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS)
            cameraPos -= cameraRight * currentSpeed;
        if(glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS)
            cameraPos += cameraRight * currentSpeed;

        if(glfwGetKey(window, GLFW_KEY_UP) == GLFW_PRESS)
            cameraPos += cameraUp * currentSpeed;
        if(glfwGetKey(window, GLFW_KEY_DOWN) == GLFW_PRESS)
            cameraPos -= cameraUp * currentSpeed;

        if(glfwGetKey(window, GLFW_KEY_LEFT) == GLFW_PRESS)
            cameraPos -= cameraRight * currentSpeed;
        if(glfwGetKey(window, GLFW_KEY_RIGHT) == GLFW_PRESS)
            cameraPos += cameraRight * currentSpeed;

        renderDepthCubemap(lightPos, 1.0f, 1000.0f);

        glViewport(0, 0, windowWidth, windowHeight);
        glClearColor(0.0f,0.0f,0.0f,1.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        glm::mat4 projection = glm::perspective(glm::radians(FoV),
            (float)windowWidth/(float)windowHeight, nearPlane, farPlane);
        glm::mat4 view = glm::lookAt(cameraPos, cameraPos + cameraFront, cameraUp);

        // skybox
        glDepthFunc(GL_LEQUAL);
        glUseProgram(skyboxProgramID);
        {
            glm::mat4 skyView = glm::mat4(glm::mat3(view));
            glUniformMatrix4fv(glGetUniformLocation(skyboxProgramID,"view"),1,GL_FALSE,glm::value_ptr(skyView));
            glUniformMatrix4fv(glGetUniformLocation(skyboxProgramID,"projection"),1,GL_FALSE,glm::value_ptr(projection));
            glBindVertexArray(skyboxVAO);
            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_CUBE_MAP, skyboxTex);
            glDrawArrays(GL_TRIANGLES,0,36);
            glBindVertexArray(0);
        }
        glDepthFunc(GL_LESS);

        // starfield
        {
            glm::vec3 modPos = glm::vec3(
                fmod(cameraPos.x, 2000.0f),
                fmod(cameraPos.y, 2000.0f),
                fmod(cameraPos.z, 2000.0f)
            );
            glm::mat4 starModel = glm::translate(glm::mat4(1.0f), -modPos);
            glm::mat4 starMVP = projection * view * starModel;

            glUseProgram(starProgramID);
            glUniformMatrix4fv(starMVP_ID,1,GL_FALSE,glm::value_ptr(starMVP));
            glUniform3f(starBaseColor_ID,1.0f,1.0f,1.0f);
            glBindVertexArray(starVAO);
            glPointSize(2.0);
            glDrawArrays(GL_POINTS,0,NUM_STARS);
            glBindVertexArray(0);
        }

        // astronaut
        {
            glm::mat4 astronautModel = glm::mat4(1.0f);
            astronautModel = glm::translate(astronautModel, glm::vec3(0,0,0));
            astronautModel = glm::scale(astronautModel, glm::vec3(astronautScale));
            glm::mat4 astronautMVP = projection * view * astronautModel;
            glm::mat3 normalMatrix = glm::mat3(glm::transpose(glm::inverse(astronautModel)));

            glUseProgram(objectProgramID);
            glUniformMatrix4fv(objectMVP_ID,1,GL_FALSE,glm::value_ptr(astronautMVP));
            glUniformMatrix4fv(objectModel_ID,1,GL_FALSE,glm::value_ptr(astronautModel));
            glUniformMatrix4fv(objectView_ID,1,GL_FALSE,glm::value_ptr(view));
            glUniformMatrix3fv(objectNormalMatrix_ID,1,GL_FALSE,glm::value_ptr(normalMatrix));
            glUniform3fv(objectLightPos_ID,1,glm::value_ptr(lightPos));
            glUniform3fv(objectViewPos_ID,1,glm::value_ptr(cameraPos));
            glUniform3f(objectBaseColor_ID,1.0f,1.0f,1.0f);

            glActiveTexture(GL_TEXTURE2);
            glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);

            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, astronautDiffuse);
            glUniform1i(objectDiffuseTex_ID,0);

            glActiveTexture(GL_TEXTURE1);
            glBindTexture(GL_TEXTURE_2D, astronautNormal);
            glUniform1i(objectNormalMap_ID,1);

            glBindVertexArray(astronautVAO);
            glDrawArrays(GL_TRIANGLES,0,36); // 36 vertices for the cube
            glBindVertexArray(0);
        }

        // asteroids (LOD)
        {
            static float asteroidAngle = 0.0f;
            asteroidAngle += deltaTime * 10.0f;

            glUseProgram(objectProgramID);
            glUniform3fv(objectLightPos_ID,1,glm::value_ptr(lightPos));
            glUniform3fv(objectViewPos_ID,1,glm::value_ptr(cameraPos));
            glUniform3f(objectBaseColor_ID,1.0f,1.0f,1.0f);

            // bind cubemap
            glActiveTexture(GL_TEXTURE2);
            glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);

            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, asteroidDiffuse);
            glUniform1i(objectDiffuseTex_ID,0);

            glActiveTexture(GL_TEXTURE1);
            glBindTexture(GL_TEXTURE_2D, asteroidNormal);
            glUniform1i(objectNormalMap_ID,1);

            // asteroid shadows and light circles
            for (auto &ast : asteroids) {
                glm::mat4 astModel(1.0f);
                astModel = glm::translate(astModel, ast.pos);
                astModel = glm::rotate(astModel, asteroidAngle*ast.rotSpeed, ast.rotAxis);
                astModel = glm::scale(astModel, glm::vec3(ast.scale));
                glm::mat4 astMVP = projection * view * astModel;
                glm::mat3 astNormalMatrix = glm::mat3(glm::transpose(glm::inverse(astModel)));

                glUniformMatrix4fv(objectMVP_ID,1,GL_FALSE,glm::value_ptr(astMVP));
                glUniformMatrix4fv(objectModel_ID,1,GL_FALSE,glm::value_ptr(astModel));
                glUniformMatrix4fv(objectView_ID,1,GL_FALSE,glm::value_ptr(view));
                glUniformMatrix3fv(objectNormalMatrix_ID,1,GL_FALSE,glm::value_ptr(astNormalMatrix));

                float dist = glm::length(ast.pos - cameraPos);
                if(dist > 120.0f) {
                    glBindVertexArray(asteroidVAO_low);
                    glDrawArrays(GL_TRIANGLES,0,(GLsizei)asteroidMesh_low.size());
                } else {
                    glBindVertexArray(asteroidVAO_high);
                    glDrawArrays(GL_TRIANGLES,0,(GLsizei)asteroidMesh_high.size());
                }
                glBindVertexArray(0);
            }
        }
        // asteroid shadows and light circles

        {
            for (auto &ast : asteroids) {
                float groundY = -20.0f + 0.01f; // just above ground
                // bigger dark circle (shadow)
                {
                    glm::mat4 circleModel(1.0f);
                    circleModel = glm::translate(circleModel, glm::vec3(ast.pos.x, groundY, ast.pos.z));
                    float shadowRadius = ast.scale * 0.8f;
                    circleModel = glm::scale(circleModel, glm::vec3(shadowRadius));
                    glm::mat4 circleMVP = projection * view * circleModel;
                    glm::vec3 shadowColor(0.2f, 0.2f, 0.2f);
                    drawCircleLight(circleMVP, shadowColor);
                }
                // smaller bright circle (light)
                {
                    // emerald island
                    glm::mat4 circleModel(1.0f);
                    circleModel = glm::translate(circleModel, glm::vec3(ast.pos.x, groundY+0.0001f, ast.pos.z));
                    float lightRadius = ast.scale * 0.5f;
                    circleModel = glm::scale(circleModel, glm::vec3(lightRadius));
                    glm::mat4 circleMVP = projection * view * circleModel;
                    glm::vec3 lightColor(1.0f, 1.0f, 0.3f);
                    drawCircleLight(circleMVP, lightColor);
                }
            }
        }

        {
            glm::mat4 emeraldModel(1.0f);
            emeraldModel = glm::translate(emeraldModel, glm::vec3(0,-20,-50));
            emeraldModel = glm::scale(emeraldModel, glm::vec3(5.0f));
            glm::mat4 emeraldMVPmat = projection * view * emeraldModel;

            glUseProgram(emeraldProgramID);
            glUniformMatrix4fv(emeraldMVP_ID,1,GL_FALSE,glm::value_ptr(emeraldMVPmat));

            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, emeraldTextureID);
            glUniform1i(emeraldTex_ID,0);

            glBindVertexArray(emeraldVAO);
            glDrawArrays(GL_TRIANGLES,0,6);
            glBindVertexArray(0);
        }

        {
            // update particles
            for(auto &p : particles) {
                p.life -= deltaTime;
                if(p.life > 0.0f) {
                    p.position += p.velocity * deltaTime;
                } else {
                    respawnParticle(p);
                }
            }
            glBindBuffer(GL_ARRAY_BUFFER, particleVBO);
            glBufferSubData(GL_ARRAY_BUFFER, 0, MAX_PARTICLES * sizeof(Particle), particles.data());
            glUseProgram(particleProgramID);
            glm::mat4 particleMVP = projection * view;
            glUniformMatrix4fv(particleMVP_ID,1,GL_FALSE,glm::value_ptr(particleMVP));
            glBindVertexArray(particleVAO);
            glEnable(GL_PROGRAM_POINT_SIZE);
            glPointSize(5.0f);
            glDrawArrays(GL_POINTS, 0, MAX_PARTICLES);
            glBindVertexArray(0);
        }
        // render character

        {
            glm::mat4 vp = projection * view;
            glUseProgram(myCharacter.getProgramID());
            glActiveTexture(GL_TEXTURE2);
            glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);
            myCharacter.render(vp);
        }

        {
            glUseProgram(objectProgramID);
            glUniform3fv(objectLightPos_ID,1,glm::value_ptr(lightPos));
            glUniform3fv(objectViewPos_ID,1,glm::value_ptr(cameraPos));
            glUniform3f(objectBaseColor_ID,1.0f,1.0f,1.0f);

            glActiveTexture(GL_TEXTURE2);
            glBindTexture(GL_TEXTURE_CUBE_MAP, depthCubemap);

            glActiveTexture(GL_TEXTURE0);
            glBindTexture(GL_TEXTURE_2D, towerTex);
            glUniform1i(objectDiffuseTex_ID,0);

            glActiveTexture(GL_TEXTURE1);
            glBindTexture(GL_TEXTURE_2D, asteroidNormal);
            glUniform1i(objectNormalMap_ID,1);

            glm::mat4 sModel(1.0f);
            sModel = glm::translate(sModel, glm::vec3(-100.0f, -20.0f, -200.0f));
            sModel = glm::scale(sModel, glm::vec3(0.1f));
            glm::mat4 sMVP = projection * view * sModel;
            glm::mat3 sNormalMatrix = glm::mat3(glm::transpose(glm::inverse(sModel)));
            glUniformMatrix4fv(objectMVP_ID,1,GL_FALSE,glm::value_ptr(sMVP));
            glUniformMatrix4fv(objectModel_ID,1,GL_FALSE,glm::value_ptr(sModel));
            glUniformMatrix4fv(objectView_ID,1,GL_FALSE,glm::value_ptr(view));
            glUniformMatrix3fv(objectNormalMatrix_ID,1,GL_FALSE,glm::value_ptr(sNormalMatrix));
            glBindVertexArray(shortBoxVAO);
            glDrawElements(GL_TRIANGLES,(GLsizei)shortBoxIndices.size(),GL_UNSIGNED_INT,(void*)0);
            glBindVertexArray(0);

            glm::mat4 tModel(1.0f);
            tModel = glm::translate(tModel, glm::vec3(100.0f, -20.0f, -200.0f));
            tModel = glm::scale(tModel, glm::vec3(0.1f));
            glm::mat4 tMVP = projection * view * tModel;
            glm::mat3 tNormalMatrix = glm::mat3(glm::transpose(glm::inverse(tModel)));
            glUniformMatrix4fv(objectMVP_ID,1,GL_FALSE,glm::value_ptr(tMVP));
            glUniformMatrix4fv(objectModel_ID,1,GL_FALSE,glm::value_ptr(tModel));
            glUniformMatrix4fv(objectView_ID,1,GL_FALSE,glm::value_ptr(view));
            glUniformMatrix3fv(objectNormalMatrix_ID,1,GL_FALSE,glm::value_ptr(tNormalMatrix));
            glBindVertexArray(tallBoxVAO);
            glDrawElements(GL_TRIANGLES,(GLsizei)tallBoxIndices.size(),GL_UNSIGNED_INT,(void*)0);
            glBindVertexArray(0);
        }

        glfwSwapBuffers(window);
    }

    glfwTerminate();
    return 0;
}

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode) {
    if(key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
}

// if (left mouse == true) move camera else move the light (like Lab 3)
static void cursor_callback(GLFWwindow *window, double xpos, double ypos) {
    if(xpos<0||xpos>=windowWidth||ypos<0||ypos>=windowHeight) return;

    if(firstMouse) {
        lastX = (float)xpos;
        lastY = (float)ypos;
        firstMouse = false;
    }

    // if left button down move cam or move light
    if(leftMouseDown) {
        float xoffset = (float)(xpos - lastX) * mouseSensitivity;
        float yoffset = (float)(lastY - ypos) * mouseSensitivity;
        lastX = (float)xpos;
        lastY = (float)ypos;

        yawAngle += xoffset;
        pitchAngle += yoffset;

        if(pitchAngle > 89.0f) pitchAngle = 89.0f;
        if(pitchAngle < -89.0f) pitchAngle = -89.0f;

        glm::vec3 front;
        front.x = cos(glm::radians(yawAngle)) * cos(glm::radians(pitchAngle));
        front.y = sin(glm::radians(pitchAngle));
        front.z = sin(glm::radians(yawAngle)) * cos(glm::radians(pitchAngle));
        cameraFront = glm::normalize(front);
    }
    else {
        float nx = (float)xpos/(float)windowWidth;
        float ny = (float)ypos/(float)windowHeight;
        nx = nx*2.f - 1.f;
        ny = 1.f - ny*2.f;

        const float scale = 250.f;
        lightPos.x = nx*scale - 278.f;
        lightPos.y = ny*scale + 278.f;
    }
}

static void mouse_button_callback(GLFWwindow* window, int button, int action, int mods) {
    if(button == GLFW_MOUSE_BUTTON_LEFT) {
        if(action == GLFW_PRESS) {
            leftMouseDown = true;
        }
        else if(action == GLFW_RELEASE) {
            leftMouseDown = false;
            firstMouse = true; // reset so don't jump
        }
    }
    if(button == GLFW_MOUSE_BUTTON_RIGHT) {
        if(action == GLFW_PRESS) {
            rightMouseDown = true;
        }
        else if(action == GLFW_RELEASE) {
            rightMouseDown = false;
        }
    }
}

static void framebuffer_size_callback(GLFWwindow* window, int width, int height) {
    windowWidth = width;
    windowHeight = height;
    glViewport(0,0,width,height);
}

GLuint loadCubemap(std::vector<std::string> faces) {
    GLuint textureID;
    glGenTextures(1,&textureID);
    glBindTexture(GL_TEXTURE_CUBE_MAP, textureID);
    int width, height, nrChannels;
    for(unsigned int i = 0; i<faces.size(); i++){
        unsigned char *data = stbi_load(faces[i].c_str(), &width,&height,&nrChannels,0);
        if(data) {
            glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i, 0,
                GL_RGB, width,height,0, GL_RGB, GL_UNSIGNED_BYTE, data);
            stbi_image_free(data);
        } else {
            std::cerr << "Cubemap texture failed to load at path: " << faces[i] << std::endl;
            stbi_image_free(data);
        }
    }
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_MIN_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_CUBE_MAP,GL_TEXTURE_WRAP_R,GL_CLAMP_TO_EDGE);

    return textureID;
}