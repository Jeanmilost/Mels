/*****************************************************************************
 * ==> QR_OpenGLHelper ------------------------------------------------------*
 *****************************************************************************
 * Description : OpenGL helper, contains some tools for OpenGL, and some     *
 *               important functions no more implemented in latest versions  *
 *               of OpenGL                                                   *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_OpenGLHelper.h"

//------------------------------------------------------------------------------
// QR_OpenGLHelper::IInterpolation - c++ cross-platform
//------------------------------------------------------------------------------
QR_OpenGLHelper::IInterpolation::IInterpolation() :
    m_Factor(0.0f),
    m_Enabled(false)
{}
//------------------------------------------------------------------------------
QR_OpenGLHelper::IInterpolation::~IInterpolation()
{}
//------------------------------------------------------------------------------
// QR_OpenGLHelper::IGLVertex - c++ cross-platform
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_OpenGLHelper::IGLVertex::IGLVertex() :
        m_VBO(0),
        m_Type(QR_Vertex::IE_VT_Unknown)
    {}
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    QR_OpenGLHelper::IGLVertex::~IGLVertex()
    {}
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
// QR_OpenGLHelper - c++ cross platform
//------------------------------------------------------------------------------
QR_OpenGLHelper::QR_OpenGLHelper()
{}
//------------------------------------------------------------------------------
QR_OpenGLHelper::~QR_OpenGLHelper()
{}
//------------------------------------------------------------------------------
#ifdef USE_OPENGL_DIRECT_MODE
    void QR_OpenGLHelper::Draw(      QR_Mesh                mesh,
                                     QR_Vertex::IEFormat    format,
                                     QR_Vertex::IECoordType coordType,
                               const QR_Vector3DP           translation,
                               const M_Precision            rotationX,
                               const M_Precision            rotationY,
                               const M_Precision            rotationZ,
                               const QR_Vector3DP           scale,
                                     bool                   deleteAfterUse)
    {
        // calculate stride
        QR_SizeT stride = (coordType == QR_Vertex::IE_VC_XYZ) ? 3 : 2;

        // do use normals array?
        if (format & QR_Vertex::IE_VF_Normals)
            stride += 3;

        // do use textures coordinates array?
        if (format & QR_Vertex::IE_VF_TexCoords)
            stride += 2;

        // do use colors array?
        if (format & QR_Vertex::IE_VF_Colors)
            stride += 4;

        // get mesh count
        const QR_SizeT count = mesh.size();

        glPushMatrix();

        // place model into 3D world
        glTranslatef(translation.m_X, translation.m_Y, translation.m_Z);
        glRotatef(QR_Algorithms::RadToDeg(rotationX), 1.0f, 0.0f, 0.0f);
        glRotatef(QR_Algorithms::RadToDeg(rotationY), 0.0f, 1.0f, 0.0f);
        glRotatef(QR_Algorithms::RadToDeg(rotationZ), 0.0f, 0.0f, 1.0f);
        glScalef(scale.m_X, scale.m_Y, scale.m_Z);

        // iterate through vertices to draw
        for (QR_SizeT i = 0; i < count; ++i)
        {
            // bind vertex array
            glEnableClientState(GL_VERTEX_ARRAY);
            glVertexPointer(3,
                            GL_FLOAT,
                            (GLsizei)(stride * sizeof(M_Precision)),
                            &mesh[i]->m_Buffer[0]);

            QR_SizeT offset = 3;

            // bind normals array
            if (format & QR_Vertex::IE_VF_Normals)
            {
                glEnableClientState(GL_NORMAL_ARRAY);
                glNormalPointer(GL_FLOAT,
                                (GLsizei)(stride * sizeof(M_Precision)),
                                &mesh[i]->m_Buffer[offset]);

                offset += 3;
            }

            // bind texture coordinates array
            if (format & QR_Vertex::IE_VF_TexCoords)
            {
                glEnableClientState(GL_TEXTURE_COORD_ARRAY);
                glTexCoordPointer(2,
                                  GL_FLOAT,
                                  (GLsizei)(stride * sizeof(M_Precision)),
                                  &mesh[i]->m_Buffer[offset]);

                offset += 2;
            }

            // bind colors array
            if (format & QR_Vertex::IE_VF_Colors)
            {
                glEnableClientState(GL_COLOR_ARRAY);
                glColorPointer(4,
                               GL_FLOAT,
                               (GLsizei)(stride * sizeof(M_Precision)),
                               &mesh[i]->m_Buffer[offset]);
            }

            // draw mesh
            switch (mesh[i]->m_Type)
            {
                case QR_Vertex::IE_VT_Triangles:
                    glDrawArrays(GL_TRIANGLES,
                                 0,
                                 (GLsizei)(mesh[i]->m_Buffer.size() / stride));
                    break;

                case QR_Vertex::IE_VT_TriangleStrip:
                    glDrawArrays(GL_TRIANGLE_STRIP,
                                 0,
                                 (GLsizei)(mesh[i]->m_Buffer.size() / stride));
                    break;

                case QR_Vertex::IE_VT_TriangleFan:
                    glDrawArrays(GL_TRIANGLE_FAN,
                                 0,
                                 (GLsizei)(mesh[i]->m_Buffer.size() / stride));
                    break;

                #ifndef __APPLE__
                        case QR_Vertex::IE_VT_Quads:
                            glDrawArrays(GL_QUADS,
                                         0,
                                         mesh[i]->m_Buffer.size() / stride);
                            break;

                        case QR_Vertex::IE_VT_QuadStrip:
                            glDrawArrays(GL_QUAD_STRIP,
                                         0,
                                         mesh[i]->m_Buffer.size() / stride);
                            break;
                #endif // __APPLE__

                case QR_Vertex::IE_VT_Unknown:
                default:
                    M_THROW_EXCEPTION("Unknown vertex type");
            }

            // unbind vertex array
            glDisableClientState(GL_VERTEX_ARRAY);

            // unbind normals array
            if (format & QR_Vertex::IE_VF_Normals)
                glDisableClientState(GL_NORMAL_ARRAY);

            // unbind texture coordinates array
            if (format & QR_Vertex::IE_VF_TexCoords)
                glDisableClientState(GL_TEXTURE_COORD_ARRAY);

            // unbind colors array
            if (format & QR_Vertex::IE_VF_Colors)
                glDisableClientState(GL_COLOR_ARRAY);

            glFlush();

            // do delete mesh after drawn it?
            if (deleteAfterUse)
                delete mesh[i];
        }

        glPopMatrix();

        // do delete mesh after drawn it?
        if (deleteAfterUse)
            mesh.clear();
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    bool QR_OpenGLHelper::Draw(const IGLMesh*               pGLMesh,
                                     QR_Vertex::IEFormat    format,
                                     QR_Vertex::IECoordType coordType,
                               const QR_Matrix16P&          modelMatrix,
                               const IInterpolation&        interpolation,
                               const QR_Shader*             pShader)
    {
        // no mesh to draw?
        if (!pGLMesh)
            return false;

        // no shader program?
        if (!pShader)
            return false;

        // get model matrix slot from shader
        GLint uniform = GetUniform(pShader, QR_Shader::IE_SA_ModelMatrix);

        // found it?
        if (uniform == -1)
            return false;

        // connect model matrix to shader
        glUniformMatrix4fv(uniform, 1, GL_FALSE, modelMatrix.GetPtr());

        if (interpolation.m_Enabled)
        {
            // get shader position attribute
            GLint interpolationAttrib = GetUniform(pShader,
                                                   QR_Shader::IE_SA_Interpolation);

            // found interpolation attribute?
            if (interpolationAttrib != -1)
                // send interpolation factor to shader program
                glUniform1f(interpolationAttrib, interpolation.m_Factor);
        }

        // get shader position attribute
        GLint posAttrib = GetAttribute(pShader, QR_Shader::IE_SA_Position);

        // found it?
        if (posAttrib == -1)
            return false;

        GLint iPosAttrib = -1;

        // get shader interpolation position attribute
        if (interpolation.m_Enabled)
            iPosAttrib = GetAttribute(pShader, QR_Shader::IE_SA_InterpolationPos);

        QR_SizeT stride;

        // is buffer interpolated?
        if (interpolation.m_Enabled)
            // if buffer is interpolated, position values are 2x higher
            stride = (coordType == QR_Vertex::IE_VC_XYZ) ? 6 : 4;
        else
            stride = (coordType == QR_Vertex::IE_VC_XYZ) ? 3 : 2;

        GLint normalAttrib  = -1;
        GLint iNormalAttrib = -1;

        // do use shader normal attribute?
        if (format & QR_Vertex::IE_VF_Normals)
        {
            // get shader normal attribute
            normalAttrib = GetAttribute(pShader, QR_Shader::IE_SA_Normal);

            // found it?
            if (normalAttrib == -1)
                return false;

            // get shader normal attribute
            if (interpolation.m_Enabled)
                iNormalAttrib = GetAttribute(pShader,
                                             QR_Shader::IE_SA_InterpolationNormal);

            // add normals to stride, 6 if buffer is interpolated, otherwise 3
            stride += interpolation.m_Enabled ? 6 : 3;
        }

        GLint uvAttrib = -1;

        // do use shader UV attribute?
        if (format & QR_Vertex::IE_VF_TexCoords)
        {
            // get shader UV attribute
            uvAttrib = GetAttribute(pShader, QR_Shader::IE_SA_Texture);

            // found it?
            if (uvAttrib == -1)
                return false;

            // add texture coordinates to stride
            stride += 2;
        }

        GLint colorAttrib = -1;

        // do use shader color attribute?
        if (format & QR_Vertex::IE_VF_Colors)
        {
            // get shader color attribute
            colorAttrib = GetAttribute(pShader, QR_Shader::IE_SA_Color);

            // found it?
            if (colorAttrib == -1)
                return false;

            // add color to stride
            stride += 4;
        }

        // get mesh count
        const QR_SizeT count = pGLMesh->size();

        // iterate through OpenGL meshes
        for (QR_SizeT i = 0; i < count; ++i)
        {
            // get OpenGL vertex
            QR_OpenGLHelper::IGLVertex* pGLVertex = (*pGLMesh)[i];

            // found it?
            if (!pGLVertex)
                continue;

            // bind OpenGL vertex buffer to draw
            glBindBuffer(GL_ARRAY_BUFFER, pGLVertex->m_VBO);

            QR_BufferSizeType offset;

            // is buffer interpolated?
            if (interpolation.m_Enabled)
                // if buffer is interpolated, position values are 2x higher
                offset = (coordType == QR_Vertex::IE_VC_XYZ) ? 6 : 4;
            else
                offset = (coordType == QR_Vertex::IE_VC_XYZ) ? 3 : 2;

            // connect vertices to vertex shader position attribute
            glEnableVertexAttribArray(posAttrib);
            glVertexAttribPointer(posAttrib,
                                  3,
                                  GL_FLOAT,
                                  GL_FALSE,
                                  stride * sizeof(M_Precision),
                                  NULL);

            // vertex buffer contains position to interpolate?
            if (iPosAttrib != -1)
            {
                // connect vertices to vertex shader position attribute
                glEnableVertexAttribArray(iPosAttrib);
                glVertexAttribPointer(iPosAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(M_Precision),
                                      (const GLvoid*)(3 * sizeof(M_Precision)));
            }

            // vertex buffer contains normals?
            if (normalAttrib != -1)
            {
                // connect the vertexes to the vertex shader normal attribute
                glEnableVertexAttribArray(normalAttrib);
                glVertexAttribPointer(normalAttrib,
                                      3,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(M_Precision),
                                      (const GLvoid*)(offset * sizeof(M_Precision)));

                // vertex buffer contains normal to interpolate?
                if (iNormalAttrib != -1)
                {
                    // connect the vertexes to the vertex shader normal attribute
                    glEnableVertexAttribArray(iNormalAttrib);
                    glVertexAttribPointer(iNormalAttrib,
                                          3,
                                          GL_FLOAT,
                                          GL_FALSE,
                                          stride * sizeof(M_Precision),
                                          (const GLvoid*)((offset + 3) * sizeof(M_Precision)));
                }

                offset += (interpolation.m_Enabled) ? 6 : 3;
            }

            // vertex buffer contains texture coordinates?
            if (uvAttrib != -1)
            {
                // connect the color to the vertex shader vColor attribute and redirect to
                // the fragment shader
                glEnableVertexAttribArray(uvAttrib);
                glVertexAttribPointer(uvAttrib,
                                      2,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(M_Precision),
                                      (const GLvoid*)(offset * sizeof(M_Precision)));

                offset += 2;
            }

            // vertex buffer contains colors?
            if (colorAttrib != -1)
            {
                // connect the color to the vertex shader vColor attribute and redirect to
                // the fragment shader
                glEnableVertexAttribArray(colorAttrib);
                glVertexAttribPointer(colorAttrib,
                                      4,
                                      GL_FLOAT,
                                      GL_FALSE,
                                      stride * sizeof(M_Precision),
                                      (const GLvoid*)(offset * sizeof(M_Precision)));
            }

            // search for buffer type to draw
            switch (pGLVertex->m_Type)
            {
                case QR_Vertex::IE_VT_Triangles:
                    // draw vertex array
                    glDrawArrays(GL_TRIANGLES, 0, pGLVertex->m_Size / stride);
                    break;

                case QR_Vertex::IE_VT_TriangleStrip:
                    // draw vertex array
                    glDrawArrays(GL_TRIANGLE_STRIP, 0, pGLVertex->m_Size / stride);
                    break;

                case QR_Vertex::IE_VT_TriangleFan:
                    // draw vertex array
                    glDrawArrays(GL_TRIANGLE_FAN, 0, pGLVertex->m_Size / stride);
                    break;

                #ifndef __APPLE__
                        case QR_Vertex::IE_VT_Quads:
                            // draw vertex array
                            glDrawArrays(GL_QUADS, 0, pGLVertex->m_Size / stride);
                            break;

                        case QR_Vertex::IE_VT_QuadStrip:
                            // draw vertex array
                            glDrawArrays(GL_QUAD_STRIP, 0, pGLVertex->m_Size / stride);
                            break;
                #endif // __APPLE__

                case QR_Vertex::IE_VT_Unknown:
                default:
                    M_THROW_EXCEPTION("Unknown vertex type");
            }
        }

        return true;
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
