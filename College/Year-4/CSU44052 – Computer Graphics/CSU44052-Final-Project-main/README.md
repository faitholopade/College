# CSU4405 Computer Graphics  
**Final Project Guideline**

**Instructor:**  
Binh-Son Hua  
Trinity College Dublin

---

## Project Overview

In the final project, you will develop a computer graphics application to showcase the techniques you have learned in the module within a single framework.

**Project Title:**  
**Toward a Futuristic Emerald Isle**

**Project Requirements:**
- **Individual Work:** This project is strictly individual. No group work is allowed.
- **Demonstration:** Your project will be demonstrated in your project deliverables. You may also be required to demonstrate your working program to the lecturer upon request.

---

## Application Features

Your application should include the following features:

- **Implementation:**
  - Implemented in C/C++.
  - Use shader-based OpenGL 3.3.

- **Performance:**
  - Achieve a minimum frame rate of **15 FPS** on the latest generation of GPU (e.g., desktop 4090).
  - Refer to Lab 4 on how frame rate is calculated and displayed in the window title.

- **Infinite Scene:**
  - Demonstrate an infinite scene.
  - Camera should be controllable using up, down, left, right keys or mouse buttons.
  - Simulate an endless effect when the camera moves, ensuring it does not go out of the scene.

- **Core Features:**
  - **Geometry Rendering**
  - **Texture Mapping**
  - **Lighting and Shadow**
  - **Animation**
  
  *(These are the four basic features covered in Lab 1, 2, 3, 4)*

- **User Interaction and Camera Control:**
  - Allow users to move around the scene using the keyboard and/or mouse.
  - **Minimum Controls:**
    - Move forwards and backwards.
    - Turn left and right.

- **Advanced Feature:**
  - Implement **one** of the following advanced features not discussed in class:
    - Deferred shading
    - Screen-space ambient occlusion
    - Screen-space depth of field
    - Environment lighting
    - Level of details
    - Instancing
    - Real-time global illumination (e.g., voxel cone tracing)
    - Physics-based animation (e.g., particle systems, smoothed particle hydrodynamics)
    - Support multi-platform graphics: Android/iOS, WebGL, AR/VR.

  *For other advanced features, discuss with the lecturer before implementation.*

---

## Deliverables

Your final submission should include:

1. **Final Report** (Max 4 pages):
    - **Introduction:** Overview of your application (purpose, implemented features, achievements).
    - **Progress Report:** Demonstrate the development over time with at least 5 screenshots capturing different stages.
    - **Quality and Robustness:** Discuss the application's quality and robustness.
    - **Limitations and Future Work:** Discuss current limitations and potential future enhancements.
    - **Acknowledgements:** Credit any peers who helped, and acknowledge any open data or source code used.

2. **Illustrative Video**:
    - Format: MP4
    - Duration: Maximum 7 minutes.
    - Content: Clearly demonstrate all features with possible voiceover and/or overlaid text/arrows.
    - **Note:** Selected videos may be uploaded to a YouTube playlist for future classes. To opt-out, email the lecturer.

3. **Source Code and Data**:
    - Package all source code and data (C++ code, shaders, model files) in a ZIP file.
    - Include a Git repository with the full history of your code development.

**Report Writing:**
- **Recommendation:** Use Overleaf and LaTeX.
- **Template:** Follow the ACM SIGGRAPH template.
    - [Overleaf Template](https://www.overleaf.com/read/vtbyjvngrzgz#e28726)
    - Trinity College Dublin provides professional Overleaf subscriptions for staff and students: [Overleaf for TCD](https://www.overleaf.com/edu/tcd)

**Submission:**
- Submit all deliverables to Blackboard.

---

## Timeline

- **Final Submission Deadline:**  
  **Sunday, December 29, 2024 at 23:59 (midnight).**

- **Late Submissions:**  
  Accepted until **Sunday, January 05, 2025 at 23:59 (midnight).**  
  After this date, the submission system will be closed. No further submissions are allowed.

---

## Evaluation Criteria

The project accounts for **60%** of the total module marks, broken down as follows:

- **Originality & Creativity:** 10%
- **Technical Quality & Complexity:** 30%
- **Robustness:** 10%
- **Report:** 10%
- **Bonus:** Up to 10% for projects demonstrating advanced feature implementations.

**Penalties:**
- **Late Submissions:** 20% penalty, capping the maximum achievable mark at 40%.
- **Missing Progress Report:** Significant deduction in technical marks.  
  *If screenshots were not captured during development, disable some code and take screenshots. Maintaining a Git history can help.*

**Academic Integrity:**
- **Strictly Prohibited:** Plagiarism, fabrication, and any form of academic dishonesty.
- **Consequences:** Penalties in project evaluation.

---

## Use of Open-Source Code and GPT/AI Models

- **Allowed:**
  - Exploring open-source and GPT-generated code to assist development.
  - Using GPT/AI models to create assets (e.g., 3D geometry, textures) with proper credits.
  - Using libraries to load models, acknowledged in the report.
  - Using libraries for special effects (e.g., physics) beyond core functionality, acknowledged in the report.

- **Not Allowed:**
  - Using GPT to generate all project deliverables.  
    *Result:* Zero mark for the project.
  - Using graphics engines (e.g., UE4, Unity) for rendering, camera transformations, etc.  
    *Reason:* The project tests your ability to program basic 3D graphics functionality covered in class.

*If in doubt, consult the lecturer or demonstrators.*

---

## Further Notes

- **OpenGL Version:**  
  You can use OpenGL version **>= 3.3**.

- **Window Frameworks:**  
  Other window frameworks like **SDL** are allowed if additional features are needed.  
  *Justification must be provided in the report.*

---
