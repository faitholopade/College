// character.cpp

#include <glad/gl.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtx/string_cast.hpp>

// GLTF model loader
#define TINYGLTF_IMPLEMENTATION
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <tiny_gltf.h>

#include <render/shader.h>

#include <vector>
#include <iostream>
#include <iomanip>
#define _USE_MATH_DEFINES
#include <math.h>

#define BUFFER_OFFSET(i) ((char *)NULL + (i))

static GLFWwindow *window;
static int windowWidth = 1024;
static int windowHeight = 768;

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode);

// Camera
static glm::vec3 eye_center(0.0f, 100.0f, 800.0f);
static glm::vec3 lookat(0.0f, 0.0f, 0.0f);
static glm::vec3 up(0.0f, 1.0f, 0.0f);
static float FoV = 45.0f;
static float zNear = 100.0f;
static float zFar = 1500.0f;

// Lighting
static glm::vec3 lightIntensity(5e6f, 5e6f, 5e6f);
static glm::vec3 lightPosition(-275.0f, 500.0f, 800.0f);

// Animation
static bool playAnimation = true;
static float playbackSpeed = 2.0f;

struct MyBot {
    // Shader variable IDs
    GLuint mvpMatrixID;
    GLuint jointMatricesID;
    GLuint lightPositionID;
    GLuint lightIntensityID;
    GLuint programID;

    tinygltf::Model model;
    struct NodeAnimationData {
        glm::vec3 translation;
        glm::quat rotation;
        glm::vec3 scale;
        bool hasTranslation;
        bool hasRotation;
        bool hasScale;
        NodeAnimationData() : translation(0.0f), rotation(1.0, 0.0, 0.0, 0.0), scale(1.0f), hasTranslation(false), hasRotation(false), hasScale(false) {}
    };
    // Each VAO corresponds to each mesh primitive in the GLTF model
    struct PrimitiveObject {
        GLuint vao;
        std::map<int, GLuint> vbos;
    };
    std::vector<PrimitiveObject> primitiveObjects;

    // Skinning
    struct SkinObject {
        // Transforms the geometry into the space of the respective joint
        std::vector<glm::mat4> inverseBindMatrices;

        // Transforms the geometry following the movement of the joints
        std::vector<glm::mat4> globalJointTransforms;

        // Combined transforms
        std::vector<glm::mat4> jointMatrices;
    };
    std::vector<SkinObject> skinObjects;

    // Animation
    struct SamplerObject {
        std::vector<float> input;
        std::vector<glm::vec4> output;
        int interpolation;
    };
    struct ChannelObject {
        int sampler;
        std::string targetPath;
        int targetNode;
    };
    struct AnimationObject {
        std::vector<SamplerObject> samplers;    // Animation data
    };
    std::vector<AnimationObject> animationObjects;

    glm::mat4 getNodeTransform(const tinygltf::Node& node) {
        glm::mat4 transform(1.0f);

        if (node.matrix.size() == 16) {
            transform = glm::make_mat4(node.matrix.data());
        } else {
            if (node.translation.size() == 3) {
                transform = glm::translate(transform, glm::vec3(node.translation[0], node.translation[1], node.translation[2]));
            }
            if (node.rotation.size() == 4) {
                glm::quat q(node.rotation[3], node.rotation[0], node.rotation[1], node.rotation[2]);
                transform *= glm::mat4_cast(q);
            }
            if (node.scale.size() == 3) {
                transform = glm::scale(transform, glm::vec3(node.scale[0], node.scale[1], node.scale[2]));
            }
        }
        return transform;
    }

    void computeLocalNodeTransform(const tinygltf::Model& model,
        int nodeIndex,
        std::vector<glm::mat4> &localTransforms)
    {
        const tinygltf::Node &node = model.nodes[nodeIndex];
        localTransforms[nodeIndex] = getNodeTransform(node);

        // Recursively compute for child nodes
        for (size_t i = 0; i < node.children.size(); ++i) {
            int childIndex = node.children[i];
            computeLocalNodeTransform(model, childIndex, localTransforms);
        }
    }

    void computeGlobalNodeTransform(const tinygltf::Model& model,
        const std::vector<glm::mat4> &localTransforms,
        int nodeIndex, const glm::mat4& parentTransform,
        std::vector<glm::mat4> &globalTransforms)
    {
        globalTransforms[nodeIndex] = parentTransform * localTransforms[nodeIndex];

        const tinygltf::Node &node = model.nodes[nodeIndex];

        // Recursively compute for child nodes
        for (size_t i = 0; i < node.children.size(); ++i) {
            int childIndex = node.children[i];
            computeGlobalNodeTransform(model, localTransforms, childIndex, globalTransforms[nodeIndex], globalTransforms);
        }
    }

    std::vector<SkinObject> prepareSkinning(const tinygltf::Model &model) {
        std::vector<SkinObject> skinObjects;

        for (size_t i = 0; i < model.skins.size(); i++) {
            SkinObject skinObject;

            const tinygltf::Skin &skin = model.skins[i];

            // Read inverseBindMatrices
            const tinygltf::Accessor &accessor = model.accessors[skin.inverseBindMatrices];
            assert(accessor.type == TINYGLTF_TYPE_MAT4);
            const tinygltf::BufferView &bufferView = model.bufferViews[accessor.bufferView];
            const tinygltf::Buffer &buffer = model.buffers[bufferView.buffer];
            const float *ptr = reinterpret_cast<const float *>(
                buffer.data.data() + accessor.byteOffset + bufferView.byteOffset);

            skinObject.inverseBindMatrices.resize(accessor.count);
            for (size_t j = 0; j < accessor.count; j++) {
                float m[16];
                memcpy(m, ptr + j * 16, 16 * sizeof(float));
                skinObject.inverseBindMatrices[j] = glm::make_mat4(m);
            }

            assert(skin.joints.size() == accessor.count);

            skinObject.globalJointTransforms.resize(skin.joints.size());
            skinObject.jointMatrices.resize(skin.joints.size());

            // Compute joint matrices
            // ----------------------------------------------
            // Compute local and global transforms
            std::vector<glm::mat4> localTransforms(model.nodes.size(), glm::mat4(1.0f));
            std::vector<glm::mat4> globalTransforms(model.nodes.size(), glm::mat4(1.0f));

            // Compute local transforms
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                int jointNodeIndex = skin.joints[j];
                computeLocalNodeTransform(model, jointNodeIndex, localTransforms);
            }

            // Compute global transforms
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                int jointNodeIndex = skin.joints[j];
                computeGlobalNodeTransform(model, localTransforms, jointNodeIndex, glm::mat4(1.0f), globalTransforms);
            }

            // Store global joint transforms
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                int jointNodeIndex = skin.joints[j];
                skinObject.globalJointTransforms[j] = globalTransforms[jointNodeIndex];
            }

            // Compute joint matrices
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                skinObject.jointMatrices[j] = skinObject.globalJointTransforms[j] * skinObject.inverseBindMatrices[j];
            }
            // ----------------------------------------------

            skinObjects.push_back(skinObject);
        }
        return skinObjects;
    }

    int findKeyframeIndex(const std::vector<float>& times, float animationTime)
    {
        int left = 0;
        int right = times.size() - 1;

        while (left <= right) {
            int mid = (left + right) / 2;

            if (mid + 1 < times.size() && times[mid] <= animationTime && animationTime < times[mid + 1]) {
                return mid;
            }
            else if (times[mid] > animationTime) {
                right = mid - 1;
            }
            else { // animationTime >= times[mid + 1]
                left = mid + 1;
            }
        }

        // Target not found
        return times.size() - 2;
    }

    std::vector<AnimationObject> prepareAnimation(const tinygltf::Model &model)
    {
        std::vector<AnimationObject> animationObjects;
        for (const auto &anim : model.animations) {
            AnimationObject animationObject;

            for (const auto &sampler : anim.samplers) {
                SamplerObject samplerObject;

                const tinygltf::Accessor &inputAccessor = model.accessors[sampler.input];
                const tinygltf::BufferView &inputBufferView = model.bufferViews[inputAccessor.bufferView];
                const tinygltf::Buffer &inputBuffer = model.buffers[inputBufferView.buffer];

                assert(inputAccessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT);
                assert(inputAccessor.type == TINYGLTF_TYPE_SCALAR);

                // Input (time) values
                samplerObject.input.resize(inputAccessor.count);

                const unsigned char *inputPtr = &inputBuffer.data[inputBufferView.byteOffset + inputAccessor.byteOffset];
                const float *inputBuf = reinterpret_cast<const float*>(inputPtr);

                // Read input (time) values
                int stride = inputAccessor.ByteStride(inputBufferView);
                for (size_t i = 0; i < inputAccessor.count; ++i) {
                    samplerObject.input[i] = *reinterpret_cast<const float*>(inputPtr + i * stride);
                }

                const tinygltf::Accessor &outputAccessor = model.accessors[sampler.output];
                const tinygltf::BufferView &outputBufferView = model.bufferViews[outputAccessor.bufferView];
                const tinygltf::Buffer &outputBuffer = model.buffers[outputBufferView.buffer];

                assert(outputAccessor.componentType == TINYGLTF_COMPONENT_TYPE_FLOAT);

                const unsigned char *outputPtr = &outputBuffer.data[outputBufferView.byteOffset + outputAccessor.byteOffset];
                const float *outputBuf = reinterpret_cast<const float*>(outputPtr);

                int outputStride = outputAccessor.ByteStride(outputBufferView);

                // Output values
                samplerObject.output.resize(outputAccessor.count);

                for (size_t i = 0; i < outputAccessor.count; ++i) {

                    if (outputAccessor.type == TINYGLTF_TYPE_VEC3) {
                        memcpy(&samplerObject.output[i], outputPtr + i * 3 * sizeof(float), 3 * sizeof(float));
                    } else if (outputAccessor.type == TINYGLTF_TYPE_VEC4) {
                        memcpy(&samplerObject.output[i], outputPtr + i * 4 * sizeof(float), 4 * sizeof(float));
                    } else {
                        std::cout << "Unsupported accessor type ..." << std::endl;
                    }

                }

                animationObject.samplers.push_back(samplerObject);
            }

            animationObjects.push_back(animationObject);
        }
        return animationObjects;
    }

    void updateAnimation(
        const tinygltf::Model &model,
        const tinygltf::Animation &anim,
        const AnimationObject &animationObject,
        float time,
        std::vector<NodeAnimationData> &nodeAnimationData)
    {
        // There are many channels so we have to accumulate the transforms
        for (const auto &channel : anim.channels) {

            int targetNodeIndex = channel.target_node;
            const auto &sampler = anim.samplers[channel.sampler];

            // Access output (value) data for the channel
            const tinygltf::Accessor &outputAccessor = model.accessors[sampler.output];
            const tinygltf::BufferView &outputBufferView = model.bufferViews[outputAccessor.bufferView];
            const tinygltf::Buffer &outputBuffer = model.buffers[outputBufferView.buffer];

            // Calculate current animation time (wrap if necessary)
            const std::vector<float> &times = animationObject.samplers[channel.sampler].input;
            float animationTime = fmod(time, times.back());

            // Find keyframe index
            int keyframeIndex = findKeyframeIndex(times, animationTime);
            int nextKeyframeIndex = keyframeIndex + 1;
            if (nextKeyframeIndex >= times.size()) {
                nextKeyframeIndex = keyframeIndex;
            }

            float t0 = times[keyframeIndex];
            float t1 = times[nextKeyframeIndex];
            float delta = t1 - t0;

            float factor = 0.0f;
            if (delta > 0.0f) {
                factor = (animationTime - t0) / delta;
            }

            const unsigned char *outputPtr = &outputBuffer.data[outputBufferView.byteOffset + outputAccessor.byteOffset];
            //const float *outputBuf = reinterpret_cast<const float*>(outputPtr);

            NodeAnimationData &nodeData = nodeAnimationData[targetNodeIndex];

            if (channel.target_path == "translation") {
                glm::vec3 translation0, translation1;
                memcpy(&translation0, outputPtr + keyframeIndex * 3 * sizeof(float), 3 * sizeof(float));
                memcpy(&translation1, outputPtr + nextKeyframeIndex * 3 * sizeof(float), 3 * sizeof(float));

                glm::vec3 translation = glm::mix(translation0, translation1, factor);
                nodeData.translation = translation;
                nodeData.hasTranslation = true;
            } else if (channel.target_path == "rotation") {
                glm::quat rotation0, rotation1;
                memcpy(&rotation0, outputPtr + keyframeIndex * 4 * sizeof(float), 4 * sizeof(float));
                memcpy(&rotation1, outputPtr + nextKeyframeIndex * 4 * sizeof(float), 4 * sizeof(float));

                glm::quat rotation = glm::slerp(rotation0, rotation1, factor);
                nodeData.rotation = rotation;
                nodeData.hasRotation = true;
            } else if (channel.target_path == "scale") {
                glm::vec3 scale0, scale1;
                memcpy(&scale0, outputPtr + keyframeIndex * 3 * sizeof(float), 3 * sizeof(float));
                memcpy(&scale1, outputPtr + nextKeyframeIndex * 3 * sizeof(float), 3 * sizeof(float));

                glm::vec3 scale = glm::mix(scale0, scale1, factor);
                nodeData.scale = scale;
                nodeData.hasScale = true;
            }
        }
    }

    void updateSkinning(const std::vector<glm::mat4> &nodeTransforms) {

        for (size_t i = 0; i < model.skins.size(); ++i) {
            SkinObject &skinObject = skinObjects[i];
            const tinygltf::Skin &skin = model.skins[i];

            // Update global joint transforms
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                int jointNodeIndex = skin.joints[j];

                // Assume that globalTransforms have been computed
                skinObject.globalJointTransforms[j] = nodeTransforms[jointNodeIndex];
            }

            // Recompute joint matrices
            for (size_t j = 0; j < skin.joints.size(); ++j) {
                skinObject.jointMatrices[j] = skinObject.globalJointTransforms[j] * skinObject.inverseBindMatrices[j];
            }
        }
    }

     void update(float time) {

    if (model.animations.size() > 0) {
        const tinygltf::Animation &animation = model.animations[0];
        const AnimationObject &animationObject = animationObjects[0];

        // Initialize nodeTransforms
        std::vector<glm::mat4> nodeTransforms(model.nodes.size(), glm::mat4(1.0f));

        // Initialize node animation data
        std::vector<NodeAnimationData> nodeAnimationData(model.nodes.size());

        // Update animation
        updateAnimation(model, animation, animationObject, time, nodeAnimationData);

        // Compose transforms
        for (size_t i = 0; i < model.nodes.size(); ++i) {
            const tinygltf::Node &node = model.nodes[i];

            glm::mat4 scaleMatrix(1.0f);
            glm::mat4 rotationMatrix(1.0f);
            glm::mat4 translationMatrix(1.0f);

            if (node.matrix.size() == 16) {
                nodeTransforms[i] = glm::make_mat4(node.matrix.data());
            } else {
                // Build scale matrix
                if (nodeAnimationData[i].hasScale) {
                    scaleMatrix = glm::scale(glm::mat4(1.0f), nodeAnimationData[i].scale);
                } else if (node.scale.size() == 3) {
                    scaleMatrix = glm::scale(glm::mat4(1.0f), glm::vec3(node.scale[0], node.scale[1], node.scale[2]));
                }

                // Build rotation matrix
                if (nodeAnimationData[i].hasRotation) {
                    rotationMatrix = glm::mat4_cast(nodeAnimationData[i].rotation);
                } else if (node.rotation.size() == 4) {
                    glm::quat q(node.rotation[3], node.rotation[0], node.rotation[1], node.rotation[2]);
                    rotationMatrix = glm::mat4_cast(q);
                }

                // Build translation matrix
                if (nodeAnimationData[i].hasTranslation) {
                    translationMatrix = glm::translate(glm::mat4(1.0f), nodeAnimationData[i].translation);
                } else if (node.translation.size() == 3) {
                    translationMatrix = glm::translate(glm::mat4(1.0f), glm::vec3(node.translation[0], node.translation[1], node.translation[2]));
                }

                // Combine the matrices in the order T * R * S
                nodeTransforms[i] = translationMatrix * rotationMatrix * scaleMatrix;
            }
        }

        // Recompute global transforms
        const tinygltf::Skin &skin = model.skins[0];
        int rootNodeIndex = skin.joints[0];
        glm::mat4 parentTransform(1.0f);
        std::vector<glm::mat4> globalTransforms(model.nodes.size(), glm::mat4(1.0f));
        computeGlobalNodeTransform(model, nodeTransforms, rootNodeIndex, parentTransform, globalTransforms);

        // Update skinning
        updateSkinning(globalTransforms);
    }
}



    bool loadModel(tinygltf::Model &model, const char *filename) {
        tinygltf::TinyGLTF loader;
        std::string err;
        std::string warn;

        bool res = loader.LoadASCIIFromFile(&model, &err, &warn, filename);
        if (!warn.empty()) {
            std::cout << "WARN: " << warn << std::endl;
        }

        if (!err.empty()) {
            std::cout << "ERR: " << err << std::endl;
        }

        if (!res)
            std::cout << "Failed to load glTF: " << filename << std::endl;
        else
            std::cout << "Loaded glTF: " << filename << std::endl;

        return res;
    }

    void initialize() {
        // Modify your path if needed
        if (!loadModel(model, "../lab4/model/bot/bot.gltf")) {
            return;
        }

        // Prepare buffers for rendering
        primitiveObjects = bindModel(model);

        // Prepare joint matrices
        skinObjects = prepareSkinning(model);

        // Prepare animation data
        animationObjects = prepareAnimation(model);

        // Create and compile our GLSL program from the shaders
        programID = LoadShadersFromFile("../lab4/shader/bot.vert", "../lab4/shader/bot.frag");
        if (programID == 0)
        {
            std::cerr << "Failed to load shaders." << std::endl;
        }

        // Get a handle for GLSL variables
        mvpMatrixID = glGetUniformLocation(programID, "MVP");
        lightPositionID = glGetUniformLocation(programID, "lightPosition");
        lightIntensityID = glGetUniformLocation(programID, "lightIntensity");
        jointMatricesID = glGetUniformLocation(programID, "jointMatrices");
    }

    void bindMesh(std::vector<PrimitiveObject> &primitiveObjects,
              tinygltf::Model &model, tinygltf::Mesh &mesh) {

    std::map<int, GLuint> vbos;
    for (size_t i = 0; i < model.bufferViews.size(); ++i) {
        const tinygltf::BufferView &bufferView = model.bufferViews[i];

        int target = bufferView.target;

        if (bufferView.target == 0) {
            // The bufferView with target == 0 in our model refers to
            // the skinning weights or other data
            continue;
        }

        const tinygltf::Buffer &buffer = model.buffers[bufferView.buffer];
        GLuint vbo;
        glGenBuffers(1, &vbo);
        glBindBuffer(target, vbo);
        glBufferData(target, bufferView.byteLength,
                    &buffer.data.at(0) + bufferView.byteOffset, GL_STATIC_DRAW);

        vbos[i] = vbo;
    }

    // Each mesh can contain several primitives (or parts), each we need to
    // bind to an OpenGL vertex array object
    for (size_t i = 0; i < mesh.primitives.size(); ++i) {

        tinygltf::Primitive primitive = mesh.primitives[i];
        tinygltf::Accessor indexAccessor = model.accessors[primitive.indices];

        GLuint vao;
        glGenVertexArrays(1, &vao);
        glBindVertexArray(vao);

        for (auto &attrib : primitive.attributes) {
            tinygltf::Accessor accessor = model.accessors[attrib.second];
            int byteStride =
                accessor.ByteStride(model.bufferViews[accessor.bufferView]);
            glBindBuffer(GL_ARRAY_BUFFER, vbos[accessor.bufferView]);

            int size = 1;
            if (accessor.type != TINYGLTF_TYPE_SCALAR) {
                size = accessor.type;
            }

            int vaa = -1;
            bool isInt = false;
            if (attrib.first == "POSITION") vaa = 0;
            else if (attrib.first == "NORMAL") vaa = 1;
            else if (attrib.first == "TEXCOORD_0") vaa = 2;
            else if (attrib.first == "JOINTS_0") {
                vaa = 3;
                isInt = true; // JOINTS_0 is an integer attribute
            }
            else if (attrib.first == "WEIGHTS_0") vaa = 4;

            if (vaa > -1) {
                glEnableVertexAttribArray(vaa);

                if (isInt) {
                    glVertexAttribIPointer(vaa, size, accessor.componentType,
                                           byteStride, BUFFER_OFFSET(accessor.byteOffset));
                } else {
                    glVertexAttribPointer(vaa, size, accessor.componentType,
                                          accessor.normalized ? GL_TRUE : GL_FALSE,
                                          byteStride, BUFFER_OFFSET(accessor.byteOffset));
                }
            } else {
                std::cout << "vaa missing: " << attrib.first << std::endl;
            }
        }

        // Record VAO for later use
        PrimitiveObject primitiveObject;
        primitiveObject.vao = vao;
        primitiveObject.vbos = vbos;
        primitiveObjects.push_back(primitiveObject);

        glBindVertexArray(0);
    }
}


    void bindModelNodes(std::vector<PrimitiveObject> &primitiveObjects,
                        tinygltf::Model &model,
                        tinygltf::Node &node) {
        // Bind buffers for the current mesh at the node
        if ((node.mesh >= 0) && (node.mesh < model.meshes.size())) {
            bindMesh(primitiveObjects, model, model.meshes[node.mesh]);
        }

        // Recursive into children nodes
        for (size_t i = 0; i < node.children.size(); i++) {
            assert((node.children[i] >= 0) && (node.children[i] < model.nodes.size()));
            bindModelNodes(primitiveObjects, model, model.nodes[node.children[i]]);
        }
    }

    std::vector<PrimitiveObject> bindModel(tinygltf::Model &model) {
        std::vector<PrimitiveObject> primitiveObjects;

        const tinygltf::Scene &scene = model.scenes[model.defaultScene];
        for (size_t i = 0; i < scene.nodes.size(); ++i) {
            assert((scene.nodes[i] >= 0) && (scene.nodes[i] < model.nodes.size()));
            bindModelNodes(primitiveObjects, model, model.nodes[scene.nodes[i]]);
        }

        return primitiveObjects;
    }

    void drawMesh(const std::vector<PrimitiveObject> &primitiveObjects,
                tinygltf::Model &model, tinygltf::Mesh &mesh) {

        for (size_t i = 0; i < mesh.primitives.size(); ++i)
        {
            GLuint vao = primitiveObjects[i].vao;
            std::map<int, GLuint> vbos = primitiveObjects[i].vbos;

            glBindVertexArray(vao);

            tinygltf::Primitive primitive = mesh.primitives[i];
            tinygltf::Accessor indexAccessor = model.accessors[primitive.indices];

            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vbos.at(indexAccessor.bufferView));

            glDrawElements(primitive.mode, indexAccessor.count,
                        indexAccessor.componentType,
                        BUFFER_OFFSET(indexAccessor.byteOffset));

            glBindVertexArray(0);
        }
    }

    void drawModelNodes(const std::vector<PrimitiveObject>& primitiveObjects,
                        tinygltf::Model &model, tinygltf::Node &node) {
        // Draw the mesh at the node, and recursively do so for children nodes
        if ((node.mesh >= 0) && (node.mesh < model.meshes.size())) {
            drawMesh(primitiveObjects, model, model.meshes[node.mesh]);
        }
        for (size_t i = 0; i < node.children.size(); i++) {
            drawModelNodes(primitiveObjects, model, model.nodes[node.children[i]]);
        }
    }
    void drawModel(const std::vector<PrimitiveObject>& primitiveObjects,
                tinygltf::Model &model) {
        // Draw all nodes
        const tinygltf::Scene &scene = model.scenes[model.defaultScene];
        for (size_t i = 0; i < scene.nodes.size(); ++i) {
            drawModelNodes(primitiveObjects, model, model.nodes[scene.nodes[i]]);
        }
    }

    void render(glm::mat4 cameraMatrix) {
    glUseProgram(programID);

    // Apply scaling to the model
    glm::mat4 modelMatrix = glm::scale(glm::mat4(1.0f), glm::vec3(3.0f)); // Adjust scale factor as needed

    // Set camera
    glm::mat4 mvp = cameraMatrix * modelMatrix;
    glUniformMatrix4fv(mvpMatrixID, 1, GL_FALSE, &mvp[0][0]);

    // Set animation data for linear blend skinning in shader
    if (!skinObjects.empty()) {
        const SkinObject &skinObject = skinObjects[0];
        glUniformMatrix4fv(jointMatricesID, skinObject.jointMatrices.size(), GL_FALSE, glm::value_ptr(skinObject.jointMatrices[0]));
    }

    // Set light data
    glUniform3fv(lightPositionID, 1, &lightPosition[0]);
    glUniform3fv(lightIntensityID, 1, &lightIntensity[0]);

    // Set material color
    GLint materialColorID = glGetUniformLocation(programID, "materialColor");
    glUniform3f(materialColorID, 1.0f, 1.0f, 1.0f); // White color

    // Draw the GLTF model
    drawModel(primitiveObjects, model);
}


    void cleanup() {
        glDeleteProgram(programID);
    }
};

int main(void)
{
    // Initialise GLFW
    if (!glfwInit())
    {
        std::cerr << "Failed to initialize GLFW." << std::endl;
        return -1;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // For MacOS
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    // Open a window and create its OpenGL context
    window = glfwCreateWindow(windowWidth, windowHeight, "Lab 4", NULL, NULL);
    if (window == NULL)
    {
        std::cerr << "Failed to open a GLFW window." << std::endl;
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);

    // Ensure we can capture the escape key being pressed below
    glfwSetInputMode(window, GLFW_STICKY_KEYS, GL_TRUE);
    glfwSetKeyCallback(window, key_callback);

    // Load OpenGL functions, gladLoadGL returns the loaded version, 0 on error.
    int version = gladLoadGL(glfwGetProcAddress);
    if (version == 0)
    {
        std::cerr << "Failed to initialize OpenGL context." << std::endl;
        return -1;
    }

    // Background
    glClearColor(0.2f, 0.2f, 0.25f, 0.0f);

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_CULL_FACE);

    // Our 3D character
    MyBot bot;
    bot.initialize();

    // Camera setup
    glm::mat4 viewMatrix, projectionMatrix;
    projectionMatrix = glm::perspective(glm::radians(FoV), (float)windowWidth / windowHeight, zNear, zFar);

    // Time and frame rate tracking
    static double lastTime = glfwGetTime();
    float time = 0.0f;            // Animation time
    float fTime = 0.0f;            // Time for measuring fps
    unsigned long frames = 0;

    // Main loop
    do
    {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        // Update states for animation
        double currentTime = glfwGetTime();
        float deltaTime = float(currentTime - lastTime);
        lastTime = currentTime;

        if (playAnimation) {
            time += deltaTime * playbackSpeed;
            bot.update(time);
        }

        // Rendering
        viewMatrix = glm::lookAt(eye_center, lookat, up);
        glm::mat4 vp = projectionMatrix * viewMatrix;
        bot.render(vp);

        // FPS tracking
        // Count number of frames over a few seconds and take average
        frames++;
        fTime += deltaTime;
        if (fTime > 2.0f) {
            float fps = frames / fTime;
            frames = 0;
            fTime = 0;

            std::stringstream stream;
            stream << std::fixed << std::setprecision(2) << "Lab 4 | Frames per second (FPS): " << fps;
            glfwSetWindowTitle(window, stream.str().c_str());
        }

        // Swap buffers
        glfwSwapBuffers(window);
        glfwPollEvents();

    } // Check if the ESC key was pressed or the window was closed
    while (!glfwWindowShouldClose(window));

    // Clean up
    bot.cleanup();

    // Close OpenGL window and terminate GLFW
    glfwTerminate();

    return 0;
}

static void key_callback(GLFWwindow *window, int key, int scancode, int action, int mode)
{
    if (key == GLFW_KEY_UP && action == GLFW_PRESS)
    {
        playbackSpeed += 1.0f;
        if (playbackSpeed > 10.0f)
            playbackSpeed = 10.0f;
    }

    if (key == GLFW_KEY_DOWN && action == GLFW_PRESS)
    {
        playbackSpeed -= 1.0f;
        if (playbackSpeed < 1.0f) {
            playbackSpeed = 1.0f;
        }
    }

    if (key == GLFW_KEY_SPACE && action == GLFW_PRESS) {
        playAnimation = !playAnimation;
    }

    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
}
