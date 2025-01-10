#include <character.h>

#include <glad/gl.h>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <tinygltf/tiny_gltf.h>
#include <render/shader.h>
#include <iostream>
#include <cstring>
#include <cassert>
#define TINYGLTF_IMPLEMENTATION
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <tinygltf/tiny_gltf.h>

#define BUFFER_OFFSET(i) ((char *)NULL + (i))

bool Character::loadModel(const std::string &filename, tinygltf::Model &model) {
    tinygltf::TinyGLTF loader;
    std::string err;
    std::string warn;

    bool res = loader.LoadASCIIFromFile(&model, &err, &warn, filename);
    if(!warn.empty()){
        std::cout << "WARN: " << warn << std::endl;
    }
    if(!err.empty()){
        std::cout << "ERR: " << err << std::endl;
    }
    if(!res) {
        std::cout << "Failed to load glTF: " << filename << std::endl;
        return false;
    }
    std::cout << "Loaded glTF: " << filename << std::endl;
    return true;
}

glm::mat4 Character::getNodeTransform(const tinygltf::Node &node) {
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

void Character::computeGlobalNodeTransform(const tinygltf::Model& model,
                                           const std::vector<glm::mat4> &localTransforms,
                                           int nodeIndex, const glm::mat4 &parentTransform,
                                           std::vector<glm::mat4> &globalTransforms)
{
    globalTransforms[nodeIndex] = parentTransform * localTransforms[nodeIndex];
    const tinygltf::Node &node = model.nodes[nodeIndex];
    for (auto childIndex : node.children) {
        computeGlobalNodeTransform(model, localTransforms, childIndex, globalTransforms[nodeIndex], globalTransforms);
    }
}

void Character::bindModelNodes(tinygltf::Model &model, int nodeIndex) {
    const tinygltf::Node &node = model.nodes[nodeIndex];
    if (node.mesh >= 0 && node.mesh < (int)model.meshes.size()) {
        const tinygltf::Mesh &mesh = model.meshes[node.mesh];
        for (size_t i = 0; i < mesh.primitives.size(); ++i) {
            const tinygltf::Primitive &primitive = mesh.primitives[i];

            // Create VAO
            GLuint vao;
            glGenVertexArrays(1, &vao);
            glBindVertexArray(vao);

            std::map<int, GLuint> vbos;

            // Indices
            const tinygltf::Accessor &indexAccessor = model.accessors[primitive.indices];
            const tinygltf::BufferView &indexBufferView = model.bufferViews[indexAccessor.bufferView];
            const tinygltf::Buffer &indexBuffer = model.buffers[indexBufferView.buffer];
            GLuint ibo;
            glGenBuffers(1, &ibo);
            glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
            glBufferData(GL_ELEMENT_ARRAY_BUFFER, indexBufferView.byteLength, &indexBuffer.data[indexBufferView.byteOffset], GL_STATIC_DRAW);

            // Attributes
            for (auto &attrib : primitive.attributes) {
                const std::string &attrName = attrib.first;
                int accessorIndex = attrib.second;
                const tinygltf::Accessor &accessor = model.accessors[accessorIndex];
                const tinygltf::BufferView &bufferView = model.bufferViews[accessor.bufferView];
                const tinygltf::Buffer &buffer = model.buffers[bufferView.buffer];

                if (vbos.find(accessor.bufferView) == vbos.end()) {
                    GLuint vbo;
                    glGenBuffers(1, &vbo);
                    vbos[accessor.bufferView] = vbo;
                    glBindBuffer(GL_ARRAY_BUFFER, vbo);
                    glBufferData(GL_ARRAY_BUFFER, bufferView.byteLength, &buffer.data[bufferView.byteOffset], GL_STATIC_DRAW);
                } else {
                    glBindBuffer(GL_ARRAY_BUFFER, vbos[accessor.bufferView]);
                }

                int size = 1;
                if (accessor.type != TINYGLTF_TYPE_SCALAR) {
                    size = accessor.type;
                }

                int vaa = -1;
                bool isInt = false;
                if (attrName == "POSITION") vaa = 0;
                else if (attrName == "NORMAL") vaa = 1;
                else if (attrName == "TEXCOORD_0") vaa = 2;
                else if (attrName == "JOINTS_0") { vaa = 3; isInt = true; }
                else if (attrName == "WEIGHTS_0") vaa = 4;

                if (vaa > -1) {
                    glEnableVertexAttribArray(vaa);
                    int byteStride = accessor.ByteStride(bufferView);
                    if (isInt) {
                        glVertexAttribIPointer(vaa, size, accessor.componentType, byteStride, BUFFER_OFFSET(accessor.byteOffset));
                    } else {
                        glVertexAttribPointer(vaa, size, accessor.componentType,
                                              accessor.normalized ? GL_TRUE : GL_FALSE, byteStride,
                                              BUFFER_OFFSET(accessor.byteOffset));
                    }
                }
            }

            PrimitiveObject po;
            po.vao = vao;
            po.vbos = vbos;
            po.indexCount = (int)indexAccessor.count;
            po.indexType = indexAccessor.componentType;
            po.indexOffset = indexAccessor.byteOffset;
            primitives.push_back(po);

            glBindVertexArray(0);
        }
    }

    for (auto c : node.children) {
        bindModelNodes(model, c);
    }
}

void Character::bindModel(tinygltf::Model &model) {
    const tinygltf::Scene &scene = model.scenes[model.defaultScene];
    for (auto nodeIndex : scene.nodes) {
        bindModelNodes(model, nodeIndex);
    }
}

std::vector<Character::AnimationObject> Character::prepareAnimations(const tinygltf::Model &model) {
    std::vector<AnimationObject> animationObjects;
    for (const auto &anim : model.animations) {
        AnimationObject animObj;
        for (auto &sampler : anim.samplers) {
            SamplerObject so;

            const tinygltf::Accessor &inputAccessor = model.accessors[sampler.input];
            const tinygltf::BufferView &inputBV = model.bufferViews[inputAccessor.bufferView];
            const tinygltf::Buffer &inputBuffer = model.buffers[inputBV.buffer];

            so.input.resize(inputAccessor.count);
            const unsigned char *inputPtr = &inputBuffer.data[inputBV.byteOffset + inputAccessor.byteOffset];

            for (size_t i=0; i<inputAccessor.count; i++) {
                so.input[i] = *reinterpret_cast<const float*>(inputPtr + i*4);
            }

            const tinygltf::Accessor &outputAccessor = model.accessors[sampler.output];
            const tinygltf::BufferView &outputBV = model.bufferViews[outputAccessor.bufferView];
            const tinygltf::Buffer &outputBuffer = model.buffers[outputBV.buffer];

            const unsigned char *outputPtr = &outputBuffer.data[outputBV.byteOffset + outputAccessor.byteOffset];
            so.output.resize(outputAccessor.count);

            int elems = 0;
            if (outputAccessor.type == TINYGLTF_TYPE_VEC3) elems = 3;
            else if (outputAccessor.type == TINYGLTF_TYPE_VEC4) elems = 4;

            for (size_t i=0; i<outputAccessor.count; i++) {
                memcpy(&so.output[i], outputPtr + i * elems * sizeof(float), elems * sizeof(float));
            }

            animObj.samplers.push_back(so);
        }
        animationObjects.push_back(animObj);
    }
    return animationObjects;
}

int Character::findKeyframeIndex(const std::vector<float>& times, float animationTime) {
    int left = 0;
    int right = (int)times.size() - 1;
    while (left <= right) {
        int mid = (left + right) / 2;
        if (mid+1 < (int)times.size() && times[mid] <= animationTime && animationTime < times[mid+1]) {
            return mid;
        } else if (times[mid] > animationTime) {
            right = mid -1;
        } else {
            left = mid+1;
        }
    }
    return (int)times.size()-2;
}

void Character::updateAnimation(const tinygltf::Model &model,
                                const tinygltf::Animation &anim,
                                const AnimationObject &animationObject,
                                float time,
                                std::vector<glm::mat4> &nodeTransforms) {

    std::vector<glm::vec3> translations(model.nodes.size(), glm::vec3(0.0f));
    std::vector<glm::quat> rotations(model.nodes.size(), glm::quat(1.0,0,0,0));
    std::vector<glm::vec3> scales(model.nodes.size(), glm::vec3(1.0f));
    std::vector<bool> hasT(model.nodes.size(), false);
    std::vector<bool> hasR(model.nodes.size(), false);
    std::vector<bool> hasS(model.nodes.size(), false);

    for (size_t c=0; c<anim.channels.size(); c++) {
        const auto &channel = anim.channels[c];
        int targetNode = channel.target_node;
        const auto &sampler = anim.samplers[channel.sampler];
        float animTime = fmod(time, animationObject.samplers[channel.sampler].input.back());
        int kf = findKeyframeIndex(animationObject.samplers[channel.sampler].input, animTime);
        int kf2 = kf+1;
        if (kf2 >= (int)animationObject.samplers[channel.sampler].input.size()) kf2=kf;
        float t0 = animationObject.samplers[channel.sampler].input[kf];
        float t1 = animationObject.samplers[channel.sampler].input[kf2];
        float dt = t1 - t0;
        float factor = 0.0f;
        if (dt > 0.0f) factor = (animTime - t0)/dt;

        const auto &out = animationObject.samplers[channel.sampler].output;
        if (channel.target_path == "translation") {
            glm::vec3 p0, p1;
            memcpy(&p0, &out[kf], 3*sizeof(float));
            memcpy(&p1, &out[kf2], 3*sizeof(float));
            glm::vec3 p = glm::mix(p0,p1,factor);
            translations[targetNode] = p;
            hasT[targetNode]=true;
        } else if (channel.target_path == "rotation") {
            glm::quat q0, q1;
            memcpy(&q0, &out[kf], 4*sizeof(float));
            memcpy(&q1, &out[kf2], 4*sizeof(float));
            glm::quat q = glm::slerp(q0,q1,factor);
            rotations[targetNode] = q;
            hasR[targetNode]=true;
        } else if (channel.target_path == "scale") {
            glm::vec3 s0, s1;
            memcpy(&s0, &out[kf], 3*sizeof(float));
            memcpy(&s1, &out[kf2], 3*sizeof(float));
            glm::vec3 s = glm::mix(s0,s1,factor);
            scales[targetNode] = s;
            hasS[targetNode]=true;
        }
    }

    // reconstruct node transforms
    for (size_t i=0; i<model.nodes.size(); i++) {
        glm::mat4 mat(1.0f);
        // base from node
        const tinygltf::Node &node = model.nodes[i];
        glm::mat4 baseMat(1.0f);
        if (node.matrix.size()==16) {
            baseMat = glm::make_mat4(node.matrix.data());
        } else {
            glm::vec3 t(0.0f); if (node.translation.size()==3) t=glm::vec3(node.translation[0],node.translation[1],node.translation[2]);
            glm::quat r(1,0,0,0); if (node.rotation.size()==4) r=glm::quat(node.rotation[3],node.rotation[0],node.rotation[1],node.rotation[2]);
            glm::vec3 sc(1.0f); if (node.scale.size()==3) sc=glm::vec3(node.scale[0],node.scale[1],node.scale[2]);
            baseMat = glm::translate(glm::mat4(1.0f),t)*glm::mat4_cast(r)*glm::scale(glm::mat4(1.0f),sc);
        }

        glm::vec3 T = hasT[i]?translations[i]:glm::vec3(0);
        glm::quat R = hasR[i]?rotations[i]:glm::quat(1,0,0,0);
        glm::vec3 S = hasS[i]?scales[i]:glm::vec3(1);

        // combine
        glm::mat4 animMat = glm::translate(glm::mat4(1.0f),T)*glm::mat4_cast(R)*glm::scale(glm::mat4(1.0f),S);

        // final
        nodeTransforms[i] = animMat;
    }
}

void Character::updateSkins(const std::vector<glm::mat4> &globalTransforms) {
    for (size_t i=0; i<model.skins.size(); i++) {
        SkinObject &skin = skins[i];
        for (size_t j=0; j<skin.joints.size(); j++) {
            int jointNodeIndex = skin.joints[j];
            skin.jointMatrices[j] = globalTransforms[jointNodeIndex] * skin.inverseBindMatrices[j];
        }
    }
}

bool Character::initialize(const std::string &gltfFilePath) {
    if(!loadModel(gltfFilePath, model)) {
        return false;
    }

    // Prepare mesh
    bindModel(model);

    // Prepare skins
    for (size_t i=0; i<model.skins.size(); i++) {
        const tinygltf::Skin &skin = model.skins[i];
        SkinObject so;
        // inverse bind
        const tinygltf::Accessor &iba = model.accessors[skin.inverseBindMatrices];
        const tinygltf::BufferView &bv = model.bufferViews[iba.bufferView];
        const tinygltf::Buffer &buf = model.buffers[bv.buffer];
        const float *pm = reinterpret_cast<const float*>(&buf.data[bv.byteOffset+iba.byteOffset]);
        so.inverseBindMatrices.resize(iba.count);
        for (size_t c=0; c<iba.count; c++) {
            float m[16];
            memcpy(m, pm+c*16,16*sizeof(float));
            so.inverseBindMatrices[c] = glm::make_mat4(m);
        }
        so.globalJointTransforms.resize(iba.count);
        so.jointMatrices.resize(iba.count);
        so.joints = skin.joints;
        skins.push_back(so);
    }

    animations = prepareAnimations(model);

    programID = LoadShadersFromFile("../finalProject/shader/bot.vert","../finaLProject/shader/bot.frag");
    if(!programID) {
        std::cerr<<"Failed to load character shaders."<<std::endl;
        return false;
    }
    mvpMatrixID = glGetUniformLocation(programID, "MVP");
    lightPositionID = glGetUniformLocation(programID, "lightPosition");
    lightIntensityID = glGetUniformLocation(programID, "lightIntensity");
    jointMatricesID = glGetUniformLocation(programID,"jointMatrices");
    materialColorID = glGetUniformLocation(programID,"materialColor");

    return true;
}

void Character::update(float time, bool playAnim, float speed) {
    // no animation or no skins, just return
    if (model.animations.empty() || model.skins.empty()) return;

    float deltaTime = time - lastUpdateTime;
    lastUpdateTime = time;
    static float animTime = 0.0f;
    if (playAnim) {
        animTime += deltaTime*speed;
    }

    const tinygltf::Animation &animation = model.animations[0];
    const AnimationObject &animationObject = animations[0];

    std::vector<glm::mat4> nodeTransforms(model.nodes.size(), glm::mat4(1.0f));

    // rebuild node transforms
    for (size_t i=0; i<model.nodes.size(); i++) {
        nodeTransforms[i] = glm::mat4(1.0f);
    }

    updateAnimation(model, animation, animationObject, animTime, nodeTransforms);

    // assume the first skin and the first joint is root
    int rootNodeIndex = model.skins[0].joints[0];
    glm::mat4 parent(1.0f);
    std::vector<glm::mat4> globalTransforms(model.nodes.size(), glm::mat4(1.0f));

    for (size_t i=0; i<model.nodes.size(); i++){
        const tinygltf::Node &node = model.nodes[i];
        glm::mat4 baseMat = getNodeTransform(node);
    }

    // hierarchical pass:
    computeGlobalNodeTransform(model, nodeTransforms, rootNodeIndex, glm::mat4(1.0f), globalTransforms);

    updateSkins(globalTransforms);
}

void Character::drawMesh(const PrimitiveObject &prim) {
    glBindVertexArray(prim.vao);
    glDrawElements(GL_TRIANGLES, prim.indexCount, prim.indexType, (void*)prim.indexOffset);
    glBindVertexArray(0);
}

void Character::drawModel(const glm::mat4 &vp) {
    //  draw
    for (auto &po : primitives) {
        drawMesh(po);
    }
}

void Character::render(const glm::mat4 &cameraMatrix) {
    glUseProgram(programID);

    glm::mat4 modelMatrix = glm::scale(glm::mat4(1.0f), glm::vec3(3.0f));
    glm::mat4 mvp = cameraMatrix * modelMatrix;
    glUniformMatrix4fv(mvpMatrixID,1,GL_FALSE,glm::value_ptr(mvp));

    glUniform3fv(lightPositionID,1,glm::value_ptr(lightPosition));
    glUniform3fv(lightIntensityID,1,glm::value_ptr(lightIntensity));

    glUniform3f(materialColorID,1.0f,1.0f,1.0f);

    if (!skins.empty()) {
        // Use first skin
        const auto &skin = skins[0];
        glUniformMatrix4fv(jointMatricesID, (GLsizei)skin.jointMatrices.size(), GL_FALSE, glm::value_ptr(skin.jointMatrices[0]));
    }

    drawModel(cameraMatrix);
}

void Character::cleanup() {
    glDeleteProgram(programID);
}
