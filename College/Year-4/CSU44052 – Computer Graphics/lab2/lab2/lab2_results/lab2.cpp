#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include <render/shader.h>
#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

#include <vector>
#include <iostream>
#include <cmath>
#include <cstdlib>

static GLFWwindow *window;
static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode);

// OpenGL camera view parameters
static glm::vec3 eye_center;
static glm::vec3 lookat(0, 0, 0);
static glm::vec3 up(0, 1, 0);

// View control
static float viewAzimuth = 0.f;
static float viewPolar = 0.f;
static float viewDistance = 300.0f;

static GLuint LoadTexture(const char *texture_file_path) {
    int w, h, channels;
    uint8_t* img = stbi_load(texture_file_path, &w, &h, &channels, 3);
    GLuint texture;
    glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    if (img) {
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, img);
        glGenerateMipmap(GL_TEXTURE_2D);
    } else {
        std::cerr << "Failed to load texture " << texture_file_path << std::endl;
    }
    stbi_image_free(img);
    return texture;
}

struct Building {
    glm::vec3 position;
    glm::vec3 scale;

    GLfloat vertex_buffer_data[72] = {
        -1.0f, -1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 1.0f,
        1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, 1.0f, -1.0f, 1.0f, 1.0f, -1.0f,
        -1.0f, -1.0f, -1.0f, -1.0f, -1.0f, 1.0f, -1.0f, 1.0f, 1.0f, -1.0f, 1.0f, -1.0f,
        1.0f, -1.0f, 1.0f, 1.0f, -1.0f, -1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 1.0f,
        -1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, 1.0f, -1.0f, -1.0f, 1.0f, -1.0f,
        -1.0f, -1.0f, -1.0f, 1.0f, -1.0f, -1.0f, 1.0f, -1.0f, 1.0f, -1.0f, -1.0f, 1.0f,
    };

    GLuint index_buffer_data[36] = {
        0, 1, 2, 0, 2, 3, // Front
        4, 5, 6, 4, 6, 7, // Back
        8, 9, 10, 8, 10, 11, // Left
        12, 13, 14, 12, 14, 15, // Right
        16, 17, 18, 16, 18, 19, // Top
        20, 21, 22, 20, 22, 23, // Bottom
    };

    GLfloat uv_buffer_data[48] = {
        0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 1.0f, 1.0f, 1.0f, 1.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f,
        0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f
    };

    GLuint vertexArrayID;
    GLuint vertexBufferID;
    GLuint indexBufferID;
    GLuint uvBufferID;
    GLuint textureID; // Add textureID for buildings
    GLuint programID;

    GLuint mvpMatrixID;
    GLuint textureSamplerID;

    void initialize(glm::vec3 position, glm::vec3 scale, const std::vector<GLuint>& textures) {
        this->position = position;
        this->scale = scale;
        glGenVertexArrays(1, &vertexArrayID);
        glBindVertexArray(vertexArrayID);
        glGenBuffers(1, &vertexBufferID);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBufferID);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_buffer_data), vertex_buffer_data, GL_STATIC_DRAW);
        glGenBuffers(1, &uvBufferID);
        glBindBuffer(GL_ARRAY_BUFFER, uvBufferID);
        glBufferData(GL_ARRAY_BUFFER, sizeof(uv_buffer_data), uv_buffer_data, GL_STATIC_DRAW);
        glGenBuffers(1, &indexBufferID);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferID);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(index_buffer_data), index_buffer_data, GL_STATIC_DRAW);
        programID = LoadShadersFromFile("../lab2/box.vert", "../lab2/box.frag");
        mvpMatrixID = glGetUniformLocation(programID, "MVP");
        textureSamplerID = glGetUniformLocation(programID, "textureSampler");
        textureID = textures[rand() % textures.size()]; // Store the texture for each building
    }

    void render(glm::mat4 cameraMatrix) {
        glUseProgram(programID);

        // Enable vertex positions
        glEnableVertexAttribArray(0);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBufferID);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

        // Enable UV coordinates
        glEnableVertexAttribArray(2);
        glBindBuffer(GL_ARRAY_BUFFER, uvBufferID);
        glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 0, 0);

        // Bind index buffer
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBufferID);

        // Apply model transformation and set the MVP matrix
        glm::mat4 modelMatrix = glm::translate(glm::mat4(), position);
        modelMatrix = glm::scale(modelMatrix, scale);
        glm::mat4 mvp = cameraMatrix * modelMatrix;
        glUniformMatrix4fv(mvpMatrixID, 1, GL_FALSE, glm::value_ptr(mvp));

        // Bind texture and set the texture sampler
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, textureID);
        glUniform1i(textureSamplerID, 0);

        // Draw the building (cube)
        glDrawElements(GL_TRIANGLES, 36, GL_UNSIGNED_INT, 0);

        // Disable the enabled attributes
        glDisableVertexAttribArray(0);
        glDisableVertexAttribArray(2);
    }

    void cleanup() {
        glDeleteBuffers(1, &vertexBufferID);
        glDeleteBuffers(1, &uvBufferID);
        glDeleteBuffers(1, &indexBufferID);
        glDeleteVertexArrays(1, &vertexArrayID);
        glDeleteTextures(1, &textureID);
        glDeleteProgram(programID);
    }
};

struct Skybox {
    GLfloat vertex_buffer_data[72] = {
        // Positions for skybox (vertices are ordered to look inwards)
        -1.0f,  1.0f, -1.0f,  -1.0f, -1.0f, -1.0f,   1.0f, -1.0f, -1.0f,   1.0f,  1.0f, -1.0f,  // Back face
         1.0f,  1.0f, -1.0f,   1.0f, -1.0f, -1.0f,   1.0f, -1.0f,  1.0f,   1.0f,  1.0f,  1.0f,  // Right face
        -1.0f, -1.0f, -1.0f,  -1.0f, -1.0f,  1.0f,   1.0f, -1.0f,  1.0f,   1.0f, -1.0f, -1.0f,  // Bottom face
        -1.0f,  1.0f,  1.0f,  -1.0f, -1.0f,  1.0f,  -1.0f, -1.0f, -1.0f,  -1.0f,  1.0f, -1.0f,  // Left face
        -1.0f, -1.0f,  1.0f,   1.0f, -1.0f,  1.0f,   1.0f,  1.0f,  1.0f,  -1.0f,  1.0f,  1.0f,  // Front face
        -1.0f,  1.0f, -1.0f,   1.0f,  1.0f, -1.0f,   1.0f,  1.0f,  1.0f,  -1.0f,  1.0f,  1.0f   // Top face
    };

    GLuint vertexArrayID;
    GLuint vertexBufferID;
    GLuint textureID; // Declare textureID for the skybox
    GLuint programID;
    GLuint mvpMatrixID;
    GLuint textureSamplerID;

    void initialize(const std::string& skyboxTexturePath) {
        glGenVertexArrays(1, &vertexArrayID);
        glBindVertexArray(vertexArrayID);

        glGenBuffers(1, &vertexBufferID);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBufferID);
        glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_buffer_data), vertex_buffer_data, GL_STATIC_DRAW);

        programID = LoadShadersFromFile("../lab2/skybox.vert", "../lab2/skybox.frag");
        mvpMatrixID = glGetUniformLocation(programID, "MVP");

        // Load the skybox texture
        textureID = LoadTexture(skyboxTexturePath.c_str());
        textureSamplerID = glGetUniformLocation(programID, "textureSampler");
    }

    void render(glm::mat4 cameraMatrix) {
        glUseProgram(programID);

        // Disable depth write for the skybox
        glDepthMask(GL_FALSE);

        // Enable vertex positions
        glBindVertexArray(vertexArrayID);
        glEnableVertexAttribArray(0);
        glBindBuffer(GL_ARRAY_BUFFER, vertexBufferID);
        glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

        // Set the MVP matrix
        glm::mat4 modelMatrix = glm::scale(glm::mat4(1.0f), glm::vec3(500.0f));
        glm::mat4 mvp = cameraMatrix * modelMatrix;
        glUniformMatrix4fv(mvpMatrixID, 1, GL_FALSE, glm::value_ptr(mvp));

        // Bind texture and set the sampler
        glActiveTexture(GL_TEXTURE0);
        glBindTexture(GL_TEXTURE_2D, textureID);
        glUniform1i(textureSamplerID, 0);

        // Draw the skybox
        glDrawArrays(GL_TRIANGLES, 0, 36);

        // Disable the vertex attribute and re-enable depth mask
        glDisableVertexAttribArray(0);
        glDepthMask(GL_TRUE);
    }

    void cleanup() {
        glDeleteBuffers(1, &vertexBufferID);
        glDeleteVertexArrays(1, &vertexArrayID);
        glDeleteTextures(1, &textureID);
        glDeleteProgram(programID);
    }
};

// Load building textures
std::vector<GLuint> loadTextures() {
    std::vector<GLuint> textures;
    for (int i = 0; i < 6; ++i) {
        std::string path = "../lab2/facade" + std::to_string(i) + ".jpg";
        textures.push_back(LoadTexture(path.c_str()));
    }
    return textures;
}

// Generate buildings
std::vector<Building> generateCity(int numBuildings, const std::vector<GLuint>& textures) {
    std::vector<Building> buildings;
    for (int i = 0; i < numBuildings; ++i) {
        glm::vec3 pos = glm::vec3((rand() % 200) - 100, 0, (rand() % 200) - 100);
        glm::vec3 scale = glm::vec3(5 + rand() % 10, 20 + rand() % 80, 5 + rand() % 10);
        Building b;
        b.initialize(pos, scale, textures);
        buildings.push_back(b);
    }
    return buildings;
}

// Main rendering loop
int main(void) {
    if (!glfwInit()) {
        std::cerr << "Failed to initialize GLFW." << std::endl;
        return -1;
    }
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    window = glfwCreateWindow(1024, 768, "Lab 2", NULL, NULL);
    if (!window) {
        std::cerr << "Failed to open a GLFW window." << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);
    glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);
    glfwSetKeyCallback(window, key_callback);
    if (gladLoadGL(glfwGetProcAddress) == 0) {
        std::cerr << "Failed to initialize OpenGL context." << std::endl;
        return -1;
    }

    // Setup projection matrix
    glm::mat4 projectionMatrix = glm::perspective(glm::radians(45.0f), 4.0f / 3.0f, 0.1f, 1000.0f);
    glClearColor(0.2f, 0.2f, 0.25f, 0.0f);
    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // Load textures and generate buildings
    std::vector<GLuint> textures = loadTextures();
    std::vector<Building> buildings = generateCity(50, textures);

    // Initialize the skybox
    Skybox skybox;
    skybox.initialize("../lab2/studio_garden.png");

    // Set initial camera position
    eye_center.y = viewDistance * cos(viewPolar);
    eye_center.x = viewDistance * cos(viewAzimuth);
    eye_center.z = viewDistance * sin(viewAzimuth);

    // Main rendering loop
    do {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glm::mat4 viewMatrix = glm::lookAt(eye_center, lookat, up);
        glm::mat4 vp = projectionMatrix * viewMatrix;

        // Render skybox first
        skybox.render(vp);

        // Render buildings
        for (auto& b : buildings) {
            b.render(vp);
        }

        glfwSwapBuffers(window);
        glfwPollEvents();
    } while (!glfwWindowShouldClose(window));

    // Cleanup
    for (auto& b : buildings) {
        b.cleanup();
    }
    skybox.cleanup();

    glfwTerminate();
    return 0;
}

void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode) {
    if (key == GLFW_KEY_R && action == GLFW_PRESS) {
        viewAzimuth = 0.f;
        viewPolar = 0.f;
        eye_center.y = viewDistance * cos(viewPolar);
        eye_center.x = viewDistance * cos(viewAzimuth);
        eye_center.z = viewDistance * sin(viewAzimuth);
        std::cout << "Reset." << std::endl;
    }
    if (key == GLFW_KEY_UP && (action == GLFW_REPEAT || action == GLFW_PRESS)) {
        viewPolar -= 0.1f;
        eye_center.y = viewDistance * cos(viewPolar);
    }
    if (key == GLFW_KEY_DOWN && (action == GLFW_REPEAT || action == GLFW_PRESS)) {
        viewPolar += 0.1f;
        eye_center.y = viewDistance * cos(viewPolar);
    }
    if (key == GLFW_KEY_LEFT && (action == GLFW_REPEAT || action == GLFW_PRESS)) {
        viewAzimuth -= 0.1f;
        eye_center.x = viewDistance * cos(viewAzimuth);
        eye_center.z = viewDistance * sin(viewAzimuth);
    }
    if (key == GLFW_KEY_RIGHT && (action == GLFW_REPEAT || action == GLFW_PRESS)) {
        viewAzimuth += 0.1f;
        eye_center.x = viewDistance * cos(viewAzimuth);
        eye_center.z = viewDistance * sin(viewAzimuth);
    }
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
        glfwSetWindowShouldClose(window, GL_TRUE);
    }
}
