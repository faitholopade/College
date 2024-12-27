#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stb/stb_image_write.h>

#include <render/shader.h> // Your loader: must have LoadShadersFromFile(...) or similar

#include <vector>
#include <iostream>
#include <cmath>

static GLFWwindow *window;
static int windowWidth = 1024;
static int windowHeight = 768;

// ============================================================
// Camera
static glm::vec3 eye_center(-278.0f, 273.0f, 800.0f);
static glm::vec3 lookat(-278.0f, 273.0f,   0.0f);
static glm::vec3 up(0.0f, 1.0f, 0.0f);

static float FoV = 45.0f;
static float zNear = 600.0f;
static float zFar  = 1500.0f;

// ============================================================
// Light
static glm::vec3 lightIntensity(60000.0f);
static glm::vec3 lightPosition(-275.0f, 500.0f, -275.0f);

static float exposure  = 1.0f;
static float shadowBias= 0.001f;
static bool enableShadows = true;

// ============================================================
// Shadow map
static GLuint depthFBO = 0;
static GLuint depthTex = 0;
static int shadowMapWidth = 0;
static int shadowMapHeight = 0;

static float depthFoV = 90.0f;
static float depthNear= 200.0f;
static float depthFar = 2000.0f;
static bool saveDepth = false;

// Forward declarations
static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode);
static void cursor_callback(GLFWwindow *window, double xpos, double ypos);
static void saveDepthTexture(GLuint fbo, std::string filename);

// ============================================================
// A helper to store geometry and GPU buffers
struct Geometry {
    std::vector<glm::vec3> positions;
    std::vector<glm::vec3> colors;
    std::vector<glm::vec3> normals;
    std::vector<unsigned int> indices;

    GLuint vao=0, vboPos=0, vboCol=0, vboNor=0, ebo=0;

    glm::mat4 modelMatrix = glm::mat4(1.0f);

    void createGL() {
        glGenVertexArrays(1, &vao);
        glBindVertexArray(vao);

        // Positions
        glGenBuffers(1, &vboPos);
        glBindBuffer(GL_ARRAY_BUFFER, vboPos);
        glBufferData(GL_ARRAY_BUFFER,
                     positions.size()*sizeof(glm::vec3),
                     positions.data(), GL_STATIC_DRAW);
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

        // Colors
        glGenBuffers(1, &vboCol);
        glBindBuffer(GL_ARRAY_BUFFER, vboCol);
        glBufferData(GL_ARRAY_BUFFER,
                     colors.size()*sizeof(glm::vec3),
                     colors.data(), GL_STATIC_DRAW);
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

        // Normals
        glGenBuffers(1, &vboNor);
        glBindBuffer(GL_ARRAY_BUFFER, vboNor);
        glBufferData(GL_ARRAY_BUFFER,
                     normals.size()*sizeof(glm::vec3),
                     normals.data(), GL_STATIC_DRAW);
        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2, 3, GL_FLOAT, GL_FALSE, 0, (void*)0);

        // Indices
        glGenBuffers(1, &ebo);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER,
                     indices.size()*sizeof(unsigned int),
                     indices.data(), GL_STATIC_DRAW);

        glBindVertexArray(0);
    }

    void drawGL() {
        glBindVertexArray(vao);
        glDrawElements(GL_TRIANGLES, (GLsizei)indices.size(), GL_UNSIGNED_INT, 0);
        glBindVertexArray(0);
    }

    void cleanup() {
        if (ebo)    glDeleteBuffers(1, &ebo);
        if (vboPos) glDeleteBuffers(1, &vboPos);
        if (vboCol) glDeleteBuffers(1, &vboCol);
        if (vboNor) glDeleteBuffers(1, &vboNor);
        if (vao)    glDeleteVertexArrays(1, &vao);
    }
};

// ============================================================
// The entire Cornell scene
class CornellScene {
public:
    Geometry floorGeom;
    Geometry ceilingGeom;
    Geometry leftGeom;
    Geometry rightGeom;
    Geometry backGeom;
    Geometry shortBox;
    Geometry tallBox;

    // Shader program
    GLuint programID = 0;
    // Uniforms
    GLint MVP_loc          = -1;
    GLint model_loc        = -1;
    GLint normal_loc       = -1;
    GLint lightPos_loc     = -1;
    GLint lightInt_loc     = -1;
    GLint shadowMap_loc    = -1;
    GLint lightMVP_loc     = -1;
    GLint shadowBias_loc   = -1;
    GLint enableShadows_loc= -1;
    GLint exposure_loc     = -1;

    void init() {
        buildWalls();
        buildShortBox();
        buildTallBox();

        floorGeom.createGL();
        ceilingGeom.createGL();
        leftGeom.createGL();
        rightGeom.createGL();
        backGeom.createGL();
        shortBox.createGL();
        tallBox.createGL();

        // Load main box shader
        programID = LoadShadersFromFile(
            "../lab3/box.vert",
            "../lab3/box.frag"
        );
        if (!programID) {
            std::cerr << "Failed to load box shaders.\n";
            exit(1);
        }

        // Look up uniform locations
        MVP_loc           = glGetUniformLocation(programID, "MVP");
        model_loc         = glGetUniformLocation(programID, "modelMatrix");
        normal_loc        = glGetUniformLocation(programID, "normalMatrix");
        lightPos_loc      = glGetUniformLocation(programID, "lightPosition");
        lightInt_loc      = glGetUniformLocation(programID, "lightIntensity");
        shadowMap_loc     = glGetUniformLocation(programID, "shadowMap");
        lightMVP_loc      = glGetUniformLocation(programID, "lightMVP");
        shadowBias_loc    = glGetUniformLocation(programID, "shadowBias");
        enableShadows_loc = glGetUniformLocation(programID, "enableShadows");
        exposure_loc      = glGetUniformLocation(programID, "exposure");
    }

    void render(const glm::mat4 &viewMat,
                const glm::mat4 &projMat,
                const glm::mat4 &lightView,
                const glm::mat4 &lightProj)
    {
        glUseProgram(programID);

        // Bind shadow map to texture unit 0
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, depthTex);
        glUniform1i(shadowMap_loc, 0);

        // Set lighting uniforms
        glUniform3fv(lightPos_loc, 1, glm::value_ptr(lightPosition));
        glUniform3fv(lightInt_loc, 1, glm::value_ptr(lightIntensity));
        glUniform1i(enableShadows_loc, (enableShadows ? 1 : 0));
        glUniform1f(shadowBias_loc, shadowBias);
        glUniform1f(exposure_loc, exposure);

        // For shadow comparison in frag
        glm::mat4 lightVP = lightProj * lightView;

        auto drawObject = [&](Geometry &g){
            glm::mat4 M   = g.modelMatrix;
            glm::mat4 MVP = projMat * viewMat * M;
            glUniformMatrix4fv(MVP_loc,    1, GL_FALSE, glm::value_ptr(MVP));
            glUniformMatrix4fv(model_loc,  1, GL_FALSE, glm::value_ptr(M));
            glm::mat4 invT = glm::transpose(glm::inverse(M));
            glUniformMatrix4fv(normal_loc, 1, GL_FALSE, glm::value_ptr(invT));
            glm::mat4 LMVP = lightVP * M;
            glUniformMatrix4fv(lightMVP_loc, 1, GL_FALSE, glm::value_ptr(LMVP));

            g.drawGL();
        };

        drawObject(floorGeom);
        drawObject(ceilingGeom);
        drawObject(leftGeom);
        drawObject(rightGeom);
        drawObject(backGeom);
        drawObject(shortBox);
        drawObject(tallBox);
    }

    void cleanup() {
        floorGeom.cleanup();
        ceilingGeom.cleanup();
        leftGeom.cleanup();
        rightGeom.cleanup();
        backGeom.cleanup();
        shortBox.cleanup();
        tallBox.cleanup();

        if (programID) glDeleteProgram(programID);
    }

private:
    // -------------------------------------------------------
    // Build the 5 walls (floor, ceiling, left, right, back)
    void buildWalls() {
        using glm::vec3;

        // Floor
        {
            floorGeom.positions = {
                vec3(-552.8f,   0.0f,   0.0f),
                vec3(   0.0f,   0.0f,   0.0f),
                vec3(   0.0f,   0.0f, -559.2f),
                vec3(-549.6f,   0.0f, -559.2f)
            };
            for(int i=0;i<4;i++){
                floorGeom.normals.push_back(vec3(0,1,0));
                floorGeom.colors.push_back(vec3(1,1,1)); // white
            }
            floorGeom.indices = {0,1,2, 0,2,3};
        }
        // Ceiling
        {
            ceilingGeom.positions = {
                vec3(-556.0f, 548.8f,    0.0f),
                vec3(-556.0f, 548.8f, -559.2f),
                vec3(   0.0f, 548.8f, -559.2f),
                vec3(   0.0f, 548.8f,    0.0f)
            };
            for(int i=0;i<4;i++){
                ceilingGeom.normals.push_back(vec3(0,-1,0));
                ceilingGeom.colors.push_back(vec3(1,1,1));
            }
            ceilingGeom.indices = {0,1,2, 0,2,3};
        }
        // Left (red)
        {
            leftGeom.positions = {
                vec3(-552.8f,    0.0f,    0.0f),
                vec3(-549.6f,    0.0f, -559.2f),
                vec3(-556.0f,  548.8f, -559.2f),
                vec3(-556.0f,  548.8f,    0.0f)
            };
            for(int i=0;i<4;i++){
                leftGeom.normals.push_back(vec3(-1,0,0));
                leftGeom.colors.push_back(vec3(1,0,0));
            }
            leftGeom.indices = {0,1,2, 0,2,3};
        }
        // Right (green)
        {
            rightGeom.positions = {
                vec3(   0.0f,    0.0f, -559.2f),
                vec3(   0.0f,    0.0f,    0.0f),
                vec3(   0.0f,  548.8f,    0.0f),
                vec3(   0.0f,  548.8f, -559.2f)
            };
            for(int i=0;i<4;i++){
                rightGeom.normals.push_back(vec3(1,0,0));
                rightGeom.colors.push_back(vec3(0,1,0));
            }
            rightGeom.indices = {0,1,2, 0,2,3};
        }
        // Back
        {
            backGeom.positions = {
                vec3(-549.6f,   0.0f, -559.2f),
                vec3(   0.0f,   0.0f, -559.2f),
                vec3(   0.0f, 548.8f, -559.2f),
                vec3(-556.0f, 548.8f, -559.2f)
            };
            for(int i=0;i<4;i++){
                backGeom.normals.push_back(vec3(0,0,-1));
                backGeom.colors.push_back(vec3(1,1,1));
            }
            backGeom.indices = {0,1,2, 0,2,3};
        }
    }

    // -------------------------------------------------------
    // Short box from official data, but we **negate X and Z**
    // to match the rest of the coordinate system
    void buildShortBox() {
        using glm::vec3;
        // Original official coords:
        //  (130,165,65), (82,165,225), (240,165,272), (290,165,114), etc.
        // We'll do -x for x and -z for z to match the sign convention:
        // E.g. shortBox top face corners in “transformed” space
        std::vector<vec3> V = {
            // top
            vec3(-130,165,-65),
            vec3( -82,165,-225),
            vec3(-240,165,-272),
            vec3(-290,165,-114),

            // bottom
            vec3(-130,  0,-65),
            vec3( -82,  0,-225),
            vec3(-240,  0,-272),
            vec3(-290,  0,-114)
        };

        // We'll add each face with a local offset
        // Top face
        {
            int startIndex = (int)shortBox.positions.size();
            shortBox.positions.push_back(V[0]);
            shortBox.positions.push_back(V[1]);
            shortBox.positions.push_back(V[2]);
            shortBox.positions.push_back(V[3]);
            for(int i=0;i<4;i++){
                shortBox.normals.push_back(vec3(0,1,0));
                shortBox.colors.push_back(vec3(1,1,1));
            }
            shortBox.indices.insert(shortBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // front face => bottom(4)->top(0)->top(3)->bottom(7)
        {
            int startIndex = (int)shortBox.positions.size();
            shortBox.positions.push_back(V[4]);
            shortBox.positions.push_back(V[0]);
            shortBox.positions.push_back(V[3]);
            shortBox.positions.push_back(V[7]);

            glm::vec3 e1 = V[0]-V[4];
            glm::vec3 e2 = V[7]-V[4];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                shortBox.normals.push_back(n);
                shortBox.colors.push_back(vec3(1,1,1));
            }
            shortBox.indices.insert(shortBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // left face => bottom(5)->top(1)->top(0)->bottom(4)
        {
            int startIndex = (int)shortBox.positions.size();
            shortBox.positions.push_back(V[5]);
            shortBox.positions.push_back(V[1]);
            shortBox.positions.push_back(V[0]);
            shortBox.positions.push_back(V[4]);

            glm::vec3 e1 = V[1]-V[5];
            glm::vec3 e2 = V[4]-V[5];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                shortBox.normals.push_back(n);
                shortBox.colors.push_back(vec3(1,1,1));
            }
            shortBox.indices.insert(shortBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // right face => bottom(6)->top(2)->top(1)->bottom(5)
        {
            int startIndex = (int)shortBox.positions.size();
            shortBox.positions.push_back(V[6]);
            shortBox.positions.push_back(V[2]);
            shortBox.positions.push_back(V[1]);
            shortBox.positions.push_back(V[5]);

            glm::vec3 e1 = V[2]-V[6];
            glm::vec3 e2 = V[5]-V[6];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                shortBox.normals.push_back(n);
                shortBox.colors.push_back(vec3(1,1,1));
            }
            shortBox.indices.insert(shortBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // back face => bottom(7)->top(3)->top(2)->bottom(6)
        {
            int startIndex = (int)shortBox.positions.size();
            shortBox.positions.push_back(V[7]);
            shortBox.positions.push_back(V[3]);
            shortBox.positions.push_back(V[2]);
            shortBox.positions.push_back(V[6]);

            glm::vec3 e1 = V[3]-V[7];
            glm::vec3 e2 = V[6]-V[7];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                shortBox.normals.push_back(n);
                shortBox.colors.push_back(vec3(1,1,1));
            }
            shortBox.indices.insert(shortBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
    }

    // -------------------------------------------------------
    // Tall box, similarly negating X and Z
    void buildTallBox() {
        using glm::vec3;
        std::vector<vec3> V = {
            // top
            vec3(-423,330,-247),
            vec3(-265,330,-296),
            vec3(-314,330,-456),
            vec3(-472,330,-406),

            // bottom
            vec3(-423,  0,-247),
            vec3(-265,  0,-296),
            vec3(-314,  0,-456),
            vec3(-472,  0,-406)
        };

        // top face
        {
            int startIndex = (int)tallBox.positions.size();
            tallBox.positions.push_back(V[0]);
            tallBox.positions.push_back(V[1]);
            tallBox.positions.push_back(V[2]);
            tallBox.positions.push_back(V[3]);
            for(int i=0;i<4;i++){
                tallBox.normals.push_back(vec3(0,1,0));
                tallBox.colors.push_back(vec3(1,1,1));
            }
            tallBox.indices.insert(tallBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // front face => bottom(4)->top(0)->top(3)->bottom(7)
        {
            int startIndex = (int)tallBox.positions.size();
            tallBox.positions.push_back(V[4]);
            tallBox.positions.push_back(V[0]);
            tallBox.positions.push_back(V[3]);
            tallBox.positions.push_back(V[7]);

            glm::vec3 e1 = V[0]-V[4];
            glm::vec3 e2 = V[7]-V[4];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                tallBox.normals.push_back(n);
                tallBox.colors.push_back(vec3(1,1,1));
            }
            tallBox.indices.insert(tallBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // right face => bottom(7)->top(3)->top(2)->bottom(6)
        {
            int startIndex = (int)tallBox.positions.size();
            tallBox.positions.push_back(V[7]);
            tallBox.positions.push_back(V[3]);
            tallBox.positions.push_back(V[2]);
            tallBox.positions.push_back(V[6]);

            glm::vec3 e1 = V[3]-V[7];
            glm::vec3 e2 = V[6]-V[7];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                tallBox.normals.push_back(n);
                tallBox.colors.push_back(vec3(1,1,1));
            }
            tallBox.indices.insert(tallBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // back face => bottom(6)->top(2)->top(1)->bottom(5)
        {
            int startIndex = (int)tallBox.positions.size();
            tallBox.positions.push_back(V[6]);
            tallBox.positions.push_back(V[2]);
            tallBox.positions.push_back(V[1]);
            tallBox.positions.push_back(V[5]);

            glm::vec3 e1 = V[2]-V[6];
            glm::vec3 e2 = V[5]-V[6];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                tallBox.normals.push_back(n);
                tallBox.colors.push_back(vec3(1,1,1));
            }
            tallBox.indices.insert(tallBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
        // left face => bottom(5)->top(1)->top(0)->bottom(4)
        {
            int startIndex = (int)tallBox.positions.size();
            tallBox.positions.push_back(V[5]);
            tallBox.positions.push_back(V[1]);
            tallBox.positions.push_back(V[0]);
            tallBox.positions.push_back(V[4]);

            glm::vec3 e1 = V[1]-V[5];
            glm::vec3 e2 = V[4]-V[5];
            glm::vec3 n  = glm::normalize(glm::cross(e1,e2));
            for(int i=0;i<4;i++){
                tallBox.normals.push_back(n);
                tallBox.colors.push_back(vec3(1,1,1));
            }
            tallBox.indices.insert(tallBox.indices.end(),
                {
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+1),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+0),
                    (unsigned int)(startIndex+2),
                    (unsigned int)(startIndex+3)
                }
            );
        }
    }
};

CornellScene scene;

// ============================================================
int main(void)
{
    // 1) Initialize GLFW
    if (!glfwInit()) {
        std::cerr << "Failed to init GLFW.\n";
        return -1;
    }
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR,3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR,3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT,GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE,GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(windowWidth, windowHeight, "Lab 3 - CornellBox", nullptr, nullptr);
    if(!window){
        std::cerr<<"Failed to open GLFW window.\n";
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    glfwSetKeyCallback(window, key_callback);
    glfwSetCursorPosCallback(window, cursor_callback);

    // 2) Load GL
    if(!gladLoadGL(glfwGetProcAddress)){
        std::cerr<<"Failed to load OpenGL.\n";
        return -1;
    }
    // Get actual framebuffer size (esp. on Mac retina)
    glfwGetFramebufferSize(window, &shadowMapWidth, &shadowMapHeight);

    // Setup basic
    glClearColor(0.2f,0.2f,0.25f,1.0f);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // 3) Scene
    scene.init();

    // 4) Create FBO for shadow pass
    {
        glGenFramebuffers(1, &depthFBO);
        glBindFramebuffer(GL_FRAMEBUFFER, depthFBO);

        glGenTextures(1, &depthTex);
        glBindTexture(GL_TEXTURE_2D, depthTex);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT,
                     shadowMapWidth, shadowMapHeight, 0,
                     GL_DEPTH_COMPONENT, GL_FLOAT, nullptr);

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

        // attach
        glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,
                               GL_TEXTURE_2D, depthTex, 0);

        glDrawBuffer(GL_NONE);
        glReadBuffer(GL_NONE);

        if(glCheckFramebufferStatus(GL_FRAMEBUFFER)!=GL_FRAMEBUFFER_COMPLETE){
            std::cerr<<"Shadow FBO incomplete.\n";
        }
        glBindFramebuffer(GL_FRAMEBUFFER,0);
    }

    // 5) A simple depth-only shader
    GLuint depthProg = LoadShadersFromFile(
        "../lab3/depth.vert",
        "../lab3/depth.frag"
    );
    if(!depthProg){
        std::cerr<<"Failed to load depth shaders.\n";
        return -1;
    }

    // Main loop
    while(!glfwWindowShouldClose(window)){
        // ============== Shadow pass (from light’s POV) =================
        if(enableShadows) {
            glUseProgram(depthProg);
            glViewport(0,0,shadowMapWidth,shadowMapHeight);
            glBindFramebuffer(GL_FRAMEBUFFER, depthFBO);
            glClear(GL_DEPTH_BUFFER_BIT);

            glm::mat4 lightView = glm::lookAt(lightPosition,
                                              glm::vec3(-278,0,-278),
                                              glm::vec3(0,1,0));
            glm::mat4 lightProj = glm::perspective(glm::radians(depthFoV),
                                  (float)shadowMapWidth/(float)shadowMapHeight,
                                  depthNear, depthFar);

            // Render each geometry
            auto renderDepth = [&](Geometry &g){
                GLint depthMVP_loc = glGetUniformLocation(depthProg,"depthMVP");
                glm::mat4 MVP = lightProj * lightView * g.modelMatrix;
                glUniformMatrix4fv(depthMVP_loc,1,GL_FALSE,glm::value_ptr(MVP));
                g.drawGL();
            };
            renderDepth(scene.floorGeom);
            renderDepth(scene.ceilingGeom);
            renderDepth(scene.leftGeom);
            renderDepth(scene.rightGeom);
            renderDepth(scene.backGeom);
            renderDepth(scene.shortBox);
            renderDepth(scene.tallBox);

            glBindFramebuffer(GL_FRAMEBUFFER,0);
        }

        // ============== Normal pass (camera POV) =================
        {
            glViewport(0,0,windowWidth,windowHeight);
            glBindFramebuffer(GL_FRAMEBUFFER,0);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

            glm::mat4 viewMat = glm::lookAt(eye_center, lookat, up);
            glm::mat4 projMat = glm::perspective(glm::radians(FoV),
                                 (float)windowWidth/(float)windowHeight,
                                 zNear,zFar);

            glm::mat4 lightView = glm::lookAt(lightPosition,
                                              glm::vec3(-278,0,-278),
                                              glm::vec3(0,1,0));
            glm::mat4 lightProj = glm::perspective(glm::radians(depthFoV),
                                     (float)shadowMapWidth/(float)shadowMapHeight,
                                     depthNear,depthFar);

            // Render the scene with shadows
            scene.render(viewMat, projMat, lightView, lightProj);
        }

        // Optionally save depth
        if(saveDepth){
            saveDepthTexture(depthFBO, "depth_texture.png");
            saveDepth = false;
        }

        glfwSwapBuffers(window);
        glfwPollEvents();
    }

    // Cleanup
    scene.cleanup();
    glDeleteProgram(depthProg);

    glDeleteFramebuffers(1,&depthFBO);
    glDeleteTextures(1,&depthTex);

    glfwTerminate();
    return 0;
}

// ============================================================
static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mods)
{
    if(action!=GLFW_PRESS && action!=GLFW_REPEAT) return;
    switch(key){
        case GLFW_KEY_ESCAPE:
            glfwSetWindowShouldClose(window,true);
            break;
        case GLFW_KEY_R:
            eye_center = glm::vec3(-278,273,800);
            lightPosition = glm::vec3(-275,500,-275);
            break;
        case GLFW_KEY_W:
            lightPosition.z -=20.f;
            break;
        case GLFW_KEY_S:
            lightPosition.z +=20.f;
            break;
        case GLFW_KEY_UP:
            eye_center.y += 20.f;
            break;
        case GLFW_KEY_DOWN:
            eye_center.y -= 20.f;
            break;
        case GLFW_KEY_LEFT:
            eye_center.x -= 20.f;
            break;
        case GLFW_KEY_RIGHT:
            eye_center.x += 20.f;
            break;
        case GLFW_KEY_D:
            enableShadows = !enableShadows;
            std::cout<<"Shadows = "<<(enableShadows?"ON":"OFF")<<"\n";
            break;
        case GLFW_KEY_Q:
            exposure = glm::max(0.1f, exposure - 0.1f);
            std::cout<<"exposure="<<exposure<<"\n";
            break;
        case GLFW_KEY_E:
            exposure += 0.1f;
            std::cout<<"exposure="<<exposure<<"\n";
            break;
        case GLFW_KEY_SPACE:
            saveDepth = true;
            break;
        default: break;
    }
}

static void cursor_callback(GLFWwindow *window, double xpos, double ypos)
{
    if(xpos<0||xpos>=windowWidth||ypos<0||ypos>=windowHeight) return;
    float nx = (float)xpos/(float)windowWidth;
    float ny = (float)ypos/(float)windowHeight;
    nx = nx*2.f-1.f;
    ny = 1.f-ny*2.f;

    const float scale = 250.f;
    lightPosition.x = nx*scale - 278.f;
    lightPosition.y = ny*scale + 278.f;
}

static void saveDepthTexture(GLuint fbo, std::string filename)
{
    int width  = shadowMapWidth;
    int height = shadowMapHeight;
    if(width==0 || height==0){
        width  = windowWidth;
        height = windowHeight;
    }
    std::vector<float> depth(width*height);
    glBindFramebuffer(GL_FRAMEBUFFER,fbo);
    glReadPixels(0,0,width,height,GL_DEPTH_COMPONENT,GL_FLOAT,depth.data());
    glBindFramebuffer(GL_FRAMEBUFFER,0);

    // Convert to 8-bit
    std::vector<unsigned char> img(width*height);
    for(int i=0;i<width*height;++i){
        float d = depth[i];
        d = (d<0.f)?0.f: ( (d>1.f)?1.f : d );
        img[i] = (unsigned char)(d*255.f);
    }
    stbi_write_png(filename.c_str(), width,height,1, img.data(), width*1);
    std::cout<<"Depth texture saved to "<<filename<<"\n";
}
