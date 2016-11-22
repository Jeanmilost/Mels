/******************************************************************************
 * ==> QR_MD2Renderer_OpenGL -------------------------------------------------*
 ******************************************************************************
 * Description : MD2 model renderer that uses OpenGL for the drawing          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD2Renderer_OpenGL.h"

// qr engine
#include "QR_Algorithms.h"
#include "QR_STDTools.h"
#include "QR_OpenGLHelper.h"

//------------------------------------------------------------------------------
// QR_MD2Renderer_OpenGL - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Renderer_OpenGL::QR_MD2Renderer_OpenGL() :
    QR_MD2Renderer()
    #ifndef USE_OPENGL_DIRECT_MODE
        ,
        m_VAO(0)
    #endif // USE_OPENGL_DIRECT_MODE
{
    #ifndef USE_OPENGL_DIRECT_MODE
        QR_OpenGLHelper::CreateVertexArrayObject(m_VAO);
    #endif // USE_OPENGL_DIRECT_MODE
}
//------------------------------------------------------------------------------
QR_MD2Renderer_OpenGL::~QR_MD2Renderer_OpenGL()
{
    #ifndef USE_OPENGL_DIRECT_MODE
        DeleteVertexBuffers();
        QR_OpenGLHelper::DeleteVertexArrayObject(m_VAO);
    #endif // USE_OPENGL_DIRECT_MODE
}
//------------------------------------------------------------------------------
void QR_MD2Renderer_OpenGL::Initialize()
{
    #ifndef USE_OPENGL_DIRECT_MODE
        // OpenGL buffers already created?
        if (m_GLFrameDict.size())
            return;

        CreateVertexBuffers();
    #endif
}
//------------------------------------------------------------------------------
void QR_MD2Renderer_OpenGL::Draw(const QR_Double& elapsedTime, QR_SizeT fps)
{
    // no model to draw?
    if (!m_pModel)
        return;

    // no model animation?
    if (!m_pAnimation)
        return;

    // get MD2 model format and coordinate type
    QR_Vertex::IEFormat    format    = m_pModel->GetVertexFormat();
    QR_Vertex::IECoordType coordType = QR_Vertex::IE_VC_XYZ;

    #ifdef USE_OPENGL_DIRECT_MODE
        QR_Mesh mesh;

        // get next mesh to draw
        if (!m_pAnimation->GetMesh(elapsedTime,
                                   fps,
                                   mesh))
            return;

        // draw mesh
        QR_OpenGLHelper::Draw(mesh,
                              format,
                              coordType,
                              m_pModel->GetTranslation(),
                              m_pModel->GetRotationX(),
                              m_pModel->GetRotationY(),
                              m_pModel->GetRotationZ(),
                              m_pModel->GetScaling(),
                              true);
    #else
        // no shader?
        if (!m_pShader)
        {
            // finalize draw operation
            FinalizeDraw();
            return;
        }

        // bind shader program
        m_pShader->Use(true);

        // call OnDrawBegin event
        if (m_fOnDrawBegin)
            m_fOnDrawBegin(this, m_pShader);

        #ifdef __APPLE__
            glBindVertexArrayOES(m_VAO);
        #else
            glBindVertexArray(m_VAO);
        #endif

        // get current animation set
        QR_OpenGLHelper::IGLFrameDictionary::iterator it =
                m_GLFrameDict.find(m_pAnimation->GetRunningAnimationName());

        // found it?
        if (it == m_GLFrameDict.end())
        {
            // finalize draw operation
            FinalizeDraw();
            return;
        }

        // get current animation info
        QR_MD2Animation::IAnimation animation =
                m_pAnimation->GetAnimation(elapsedTime, fps);

        // found it?
        if (animation.m_FrameIndex == M_MD2_Invalid_Index)
        {
            // finalize draw operation
            FinalizeDraw();
            return;
        }

        // get OpenGL frames
        QR_OpenGLHelper::IGLFrames* pGLFrames = it->second;

        // found them?
        if (!pGLFrames)
        {
            // finalize draw operation
            FinalizeDraw();
            return;
        }

        // get OpenGL mesh
        QR_OpenGLHelper::IGLMesh* pGLMesh =
                (*pGLFrames)[animation.m_FrameIndex - m_pAnimation->GetRunningAnimationStartIndex()];

        // found it?
        if (!pGLMesh)
        {
            // finalize draw operation
            FinalizeDraw();
            return;
        }

        // populate interpolation info
        QR_OpenGLHelper::IInterpolation interpolation;
        interpolation.m_Factor  = animation.m_InterpolationFactor;
        interpolation.m_Enabled = true;

        // draw mesh
        QR_OpenGLHelper::Draw(pGLMesh,
                              format,
                              coordType,
                              m_pModel->GetMatrix(),
                              interpolation,
                              m_pShader);

        // finalize draw operation
        FinalizeDraw();
    #endif // USE_OPENGL_DIRECT_MODE
}
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_MD2Renderer_OpenGL::FinalizeDraw()
    {
        // call OnDrawEnd event
        if (m_fOnDrawEnd)
            m_fOnDrawEnd(this);

        // unbind vertex buffer object
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        // unbind vertex array object
        #ifdef __APPLE__
            glBindVertexArrayOES(0);
        #else
            glBindVertexArray(0);
        #endif

        // is shader program defined?
        if (m_pShader)            // unbind shader program
            m_pShader->Use(false);
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_MD2Renderer_OpenGL::CreateVertexBuffers()
    {
        // already created?
        if (!m_GLFrameDict.size())
            // delete previous buffers first
            DeleteVertexBuffers();

        if (!m_pAnimation)
            return;

        QR_MD2Animation::IFrameDictionary frameDict;

        // get complete MD2 animation frames
        if (!m_pAnimation->GetFrames(frameDict))
            return;

        // iterate through all animations
        for (QR_MD2Animation::IFrameDictionary::const_iterator it = frameDict.begin();
             it != frameDict.end();
             ++it)
        {
            // get source animation frames
            QR_MD2Animation::IFrames* pFrames = it->second;

            // found them?
            if (!pFrames)
                continue;

            // create OpenGL animation frames container
            std::auto_ptr<QR_OpenGLHelper::IGLFrames> pGLFrames(new QR_OpenGLHelper::IGLFrames());

            // get source mesh count
            QR_SizeT meshCount = pFrames->size();

            // iterate through meshes to create
            for (QR_SizeT i = 0; i < meshCount; ++i)
            {
                // get source mesh
                QR_Mesh* pMesh = (*pFrames)[i];

                // found it?
                if (!pMesh)
                    continue;

                // create OpenGL mesh
                std::auto_ptr<QR_OpenGLHelper::IGLMesh> pGLMesh(new QR_OpenGLHelper::IGLMesh());

                // get vertex buffer count
                const QR_SizeT vbCount = pMesh->size();

                // iterate through vertices to create
                for (QR_SizeT j = 0; j < vbCount; ++j)
                {
                    // get source vertices
                    QR_Vertex* pVertex = (*pMesh)[j];

                    // found it?
                    if (!pVertex)
                        continue;

                    // create and populate OpenGL vertices
                    std::auto_ptr<QR_OpenGLHelper::IGLVertex> pGLVertex(new QR_OpenGLHelper::IGLVertex());
                    glGenBuffers(1, &pGLVertex->m_VBO);
                    pGLVertex->m_Type = pVertex->m_Type;
                    pGLVertex->m_Size = pVertex->m_Buffer.size();

                    // create and bind OpenGL vertex buffer object
                    glBindBuffer(GL_ARRAY_BUFFER, pGLVertex->m_VBO);
                    glBufferData(GL_ARRAY_BUFFER,
                                 pGLVertex->m_Size * sizeof(M_Precision),
                                 &pVertex->m_Buffer[0],
                                 GL_STATIC_DRAW);

                    // add OpenGL vertices to mesh
                    pGLMesh->push_back(pGLVertex.get());
                    pGLVertex.release();
                }

                // add OpenGL mesh to frames, and delete source meshes
                pGLFrames->push_back(pGLMesh.get());
                pGLMesh.release();
                QR_STDTools::DelAndClear(*pMesh);
            }

            // add OpenGL frames to animation dictionary, and delete source frames
            m_GLFrameDict[it->first] = pGLFrames.get();
            pGLFrames.release();
            QR_STDTools::DelAndClear(*pFrames);
        }

        // delete source animation dictionary
        QR_STDTools::DelAndClear(frameDict);
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
#ifndef USE_OPENGL_DIRECT_MODE
    void QR_MD2Renderer_OpenGL::DeleteVertexBuffers()
    {
        // already deleted?
        if (!m_GLFrameDict.size())
            return;

        // unbind OpenGL vertex buffer object
        glBindBuffer(GL_ARRAY_BUFFER, 0);

        // iterate through animations to delete
        for (QR_OpenGLHelper::IGLFrameDictionary::const_iterator it = m_GLFrameDict.begin();
             it != m_GLFrameDict.end();
             ++it)
        {
            // get animation frames
            QR_OpenGLHelper::IGLFrames* pFrames = it->second;

            // found them?
            if (!pFrames)
                continue;

            // get mesh count
            QR_SizeT meshCount = pFrames->size();

            // iterate through meshes to delete
            for (QR_SizeT i = 0; i < meshCount; ++i)
            {
                // get mesh
                QR_OpenGLHelper::IGLMesh* pMesh = (*pFrames)[i];

                // found it?
                if (!pMesh)
                    continue;

                // get vertex buffer count
                const QR_SizeT vbCount = pMesh->size();

                // iterate through vertices to delete
                for (QR_SizeT j = 0; j < vbCount; ++j)
                {
                    // get source vertices
                    QR_OpenGLHelper::IGLVertex* pVertex = (*pMesh)[j];

                    // found it?
                    if (!pVertex)
                        continue;

                    glDeleteBuffers(1, &pVertex->m_VBO);
                }

                // delete OpenGL mesh
                QR_STDTools::DelAndClear(*pMesh);
            }

            // delete OpenGL frames
            QR_STDTools::DelAndClear(*pFrames);
        }

        // delete animation dictionary
        QR_STDTools::DelAndClear(m_GLFrameDict);
    }
#endif // USE_OPENGL_DIRECT_MODE
//------------------------------------------------------------------------------
