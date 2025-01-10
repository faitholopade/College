#ifndef CHARACTER_H
#define CHARACTER_H

#include <string>
#include <vector>
#include <glm/glm.hpp>
#include <tinygltf/tiny_gltf.h>
#include "glad/gl.h"

class Character {
public:
    // Initialize the character (load model, setup skinning, etc.)
    bool initialize(const std::string &gltfFilePath);

    //  animation states
    void update(float time, bool playAnimation, float playbackSpeed);

    // Render  character using  VP (view-projection) matrix
    void render(const glm::mat4 &cameraMatrix);

    // Cleanup
    void cleanup();

    // Control of animation
    void setPlayAnimation(bool play) { playAnimation = play; }
    void setPlaybackSpeed(float speed) { playbackSpeed = speed; }
    // for prog id
    GLuint getProgramID() const { return programID; }

private:
    struct PrimitiveObject {
        GLuint vao;
        std::map<int, GLuint> vbos;
        int indexCount;
        GLenum indexType;
        size_t indexOffset;
    };

    struct SkinObject {
        std::vector<glm::mat4> inverseBindMatrices;
        std::vector<glm::mat4> globalJointTransforms;
        std::vector<glm::mat4> jointMatrices;
        std::vector<int> joints;
    };

    struct SamplerObject {
        std::vector<float> input;
        std::vector<glm::vec4> output;
        int interpolation;
    };

    struct AnimationObject {
        std::vector<SamplerObject> samplers;
    };

    tinygltf::Model model;
    std::vector<PrimitiveObject> primitives;
    std::vector<SkinObject> skins;
    std::vector<AnimationObject> animations;

    GLuint programID;
    GLint mvpMatrixID, jointMatricesID;
    GLint lightPositionID, lightIntensityID;
    GLint materialColorID;

    glm::vec3 lightPosition = glm::vec3(-275.0f, 500.0f, 800.0f);
    glm::vec3 lightIntensity = glm::vec3(5e6f, 5e6f, 5e6f);

    bool playAnimation = true;
    float playbackSpeed = 2.0f;

    float lastUpdateTime = 0.0f;

    // Functions
    static bool loadModel(const std::string &filename, tinygltf::Model &model);
    void bindModelNodes(tinygltf::Model &model, int nodeIndex);
    void bindModel(tinygltf::Model &model);
    void drawModel(const glm::mat4 &vp);
    void drawMesh(const PrimitiveObject &prim);
    glm::mat4 getNodeTransform(const tinygltf::Node &node);
    void computeGlobalNodeTransform(const tinygltf::Model &model, const std::vector<glm::mat4> &localTransforms,
                                    int nodeIndex, const glm::mat4 &parentTransform, std::vector<glm::mat4> &globalTransforms);

    std::vector<AnimationObject> prepareAnimations(const tinygltf::Model &model);
    int findKeyframeIndex(const std::vector<float>& times, float animationTime);
    void updateAnimation(const tinygltf::Model &model,
                         const tinygltf::Animation &anim,
                         const AnimationObject &animationObject,
                         float time,
                         std::vector<glm::mat4> &nodeTransforms);
    void updateSkins(const std::vector<glm::mat4> &globalTransforms);
};

#endif
