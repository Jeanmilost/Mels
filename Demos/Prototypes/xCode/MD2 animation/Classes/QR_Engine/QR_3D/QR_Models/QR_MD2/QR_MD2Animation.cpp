/******************************************************************************
 * ==> QR_MD2Animation -------------------------------------------------------*
 ******************************************************************************
 * Description : Helper class for MD2 model                                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/


#include "QR_MD2Animation.h"

// qr engine
#include "QR_Exception.h"
#include "QR_STDTools.h"

//------------------------------------------------------------------------------
// QR_MD2Animation::IAnimation - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Animation::IAnimation::IAnimation() :
    m_FrameIndex(M_MD2_Invalid_Index),
    m_InterpolationFrameIndex(M_MD2_Invalid_Index),
    m_InterpolationFactor(0.0f)
{}
//------------------------------------------------------------------------------
QR_MD2Animation::IAnimation::IAnimation(QR_SizeT frameIndex, QR_SizeT interpolationFrameIndex,
        const QR_Double& interpolationFactor) :
    m_FrameIndex(frameIndex),
    m_InterpolationFrameIndex(interpolationFrameIndex),
    m_InterpolationFactor(interpolationFactor)
{}
//------------------------------------------------------------------------------
QR_MD2Animation::IAnimation::~IAnimation()
{}
//------------------------------------------------------------------------------
// QR_MD2Animation::IInfo - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Animation::IInfo::IInfo() : m_Start(0), m_End(0)
{}
//------------------------------------------------------------------------------
QR_MD2Animation::IInfo::IInfo(QR_SizeT start, QR_SizeT end) : m_Start(start), m_End(end)
{}
//------------------------------------------------------------------------------
QR_MD2Animation::IInfo::~IInfo()
{}
//------------------------------------------------------------------------------
// QR_MD2Animation - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Animation::QR_MD2Animation(QR_MD2* pModel, bool useCache) :
    m_pModel(pModel),
    m_pInfo(NULL),
    m_FrameIndex(0),
    m_InterpolationFactor(0.0f)
{
    // cannot create an animation without model
    M_ASSERT(m_pModel);

    // can use cache?
    if (useCache)
        // initialize cache
        m_Cache.resize(m_pModel->GetMeshCount());
}
//------------------------------------------------------------------------------
QR_MD2Animation::~QR_MD2Animation()
{
    QR_STDTools::DelAndClear(m_InfoDictionary);

    // get cache count
    const QR_SizeT cacheCount = m_Cache.size();

    // iterate through cache items
    for (QR_SizeT i = 0; i < cacheCount; ++i)
        // delete cache item
        QR_STDTools::DelAndClear(m_Cache[i]);

    m_Cache.clear();
}
//------------------------------------------------------------------------------
void QR_MD2Animation::SetUseCache(bool value)
{
    // get cache count
    const QR_SizeT cacheCount = m_Cache.size();

    // nothing to do?
    if ((!value && !cacheCount) || (value && cacheCount))
        return;

    // iterate through cache items
    for (QR_SizeT i = 0; i < cacheCount; ++i)
        // delete cache item
        QR_STDTools::DelAndClear(m_Cache[i]);

    m_Cache.clear();

    // do enable cache?
    if (value)
        // initialize cache
        m_Cache.resize(m_pModel->GetMeshCount());
}
//------------------------------------------------------------------------------
bool QR_MD2Animation::Add(const std::string& name, QR_SizeT start, QR_SizeT end)
{
    // no name?
    if (name.empty())
        return false;

    // no linked MD2 model?
    if (!m_pModel)
        return false;

    // get linked model mesh count
    const QR_SizeT meshCount = m_pModel->GetMeshCount();

    // are start or end values out of bounds?
    if (start >= meshCount || end >= meshCount)
        return false;

    // search for already existing animation
    IInfoDictionary::iterator it = m_InfoDictionary.find(name);

    // animation already exists?
    if (it != m_InfoDictionary.end())
    {
        // update animation info
        it->second->m_Start = start;
        it->second->m_End   = end;
        return true;
    }

    // create and populate new animation info
    std::auto_ptr<IInfo> pInfo(new IInfo(start, end));

    // add animation info to dictionary
    m_InfoDictionary[name] = pInfo.get();
    pInfo.release();

    return true;
}
//------------------------------------------------------------------------------
void QR_MD2Animation::Delete(const std::string& name)
{
    // no name?
    if (name.empty())
        return;

    // search for animation to delete
    IInfoDictionary::iterator it = m_InfoDictionary.find(name);

    // found it?
    if (it == m_InfoDictionary.end())
        return;

    // deleting current animation?
    if (m_pInfo == it->second)
    {
        // reset animation
        m_pInfo               = NULL;
        m_FrameIndex          = 0;
        m_InterpolationFactor = 0.0f;

        m_CurAnimName.clear();
    }

    // delete animation
    delete it->second;
    m_InfoDictionary.erase(it);
}
//------------------------------------------------------------------------------
bool QR_MD2Animation::Set(const std::string& name)
{
    // no name?
    if (name.empty())
        return false;

    // search for animation to set
    IInfoDictionary::iterator it = m_InfoDictionary.find(name);

    // found it?
    if (it == m_InfoDictionary.end())
        return false;

    // set animation
    m_pInfo               = it->second;
    m_FrameIndex          = m_pInfo->m_Start;
    m_InterpolationFactor = 0.0f;
    m_CurAnimName         = name;
    return true;
}
//------------------------------------------------------------------------------
std::string QR_MD2Animation::GetRunningAnimationName() const
{
    return m_CurAnimName;
}
//------------------------------------------------------------------------------
QR_SizeT QR_MD2Animation::GetRunningAnimationStartIndex() const
{
    // no running animation?
    if (!m_pInfo)
        return 0;

    return m_pInfo->m_Start;
}
//------------------------------------------------------------------------------
QR_SizeT QR_MD2Animation::GetRunningAnimationEndIndex() const
{
    // no running animation?
    if (!m_pInfo)
        return 0;

    return m_pInfo->m_End;
}
//------------------------------------------------------------------------------
QR_MD2Animation::IAnimation QR_MD2Animation::GetAnimation(const QR_Double& elapsedTime,
                                                                QR_SizeT   fps)
{
    // no animation info?
    if (!m_pInfo)
        return IAnimation();

    // calculate time interval between each frames
    const QR_Double timeInterval = (1000.0f / (QR_Double)fps);

    // calculate next interpolation position
    m_InterpolationFactor += (elapsedTime / timeInterval);

    // max interpolation factor reached?
    if (m_InterpolationFactor >= 1.0f)
    {
        // calculate number of frames to skip
        const QR_SizeT framesToSkip = std::floor(m_InterpolationFactor);

        // update interpolation factor (should always be between 0 and 1)
        m_InterpolationFactor -= framesToSkip;

        // skip frames
        m_FrameIndex += framesToSkip;
    }

    // calculate animation length
    const QR_SizeT animationLength = (m_pInfo->m_End - m_pInfo->m_Start) + 1;

    // current frame end reached?
    if (m_FrameIndex > m_pInfo->m_End)
        m_FrameIndex = m_pInfo->m_Start + ((m_FrameIndex - m_pInfo->m_Start) % animationLength);

    // calculate interpolation frame index, normally frame index + 1, except if hit animation end
    QR_SizeT interpolationFrameIndex = m_FrameIndex + 1;

    // next frame end reached?
    if (interpolationFrameIndex > m_pInfo->m_End)
        interpolationFrameIndex = m_pInfo->m_Start + ((interpolationFrameIndex - m_pInfo->m_Start)
                % animationLength);

    // return animation data
    return IAnimation(m_FrameIndex, interpolationFrameIndex, m_InterpolationFactor);
}
//------------------------------------------------------------------------------
bool QR_MD2Animation::GetMesh(const QR_Double& elapsedTime,
                                    QR_SizeT   fps,
                                    QR_Mesh&   mesh)
{
    // no model?
    if (!m_pModel)
        return false;

    // get animation
    IAnimation animation = GetAnimation(elapsedTime, fps);

    // found it?
    if (animation.m_FrameIndex == M_MD2_Invalid_Index)
        return false;

    // can use cache?
    if (!m_Cache.size())
    {
        QR_Mesh frameMesh;
        QR_Mesh interpolationMesh;
        bool    success = false;

        try
        {
            // get frame mesh and mesh to interpolate with
            m_pModel->GetMesh(animation.m_FrameIndex, frameMesh);
            m_pModel->GetMesh(animation.m_InterpolationFrameIndex, interpolationMesh);

            // process interpolation and send resulting mesh to output
            success = (QR_MD2::Interpolate(animation.m_InterpolationFactor,
                                           frameMesh,
                                           interpolationMesh,
                                           mesh) == QR_MD2Common::IE_C_Success);
        }
        catch (...)
        {}

        // clear memory
        QR_STDTools::DelAndClear(frameMesh);
        QR_STDTools::DelAndClear(interpolationMesh);

        return success;
    }

    // get and cache frame mesh
    if (!m_Cache[animation.m_FrameIndex].size())
        m_pModel->GetMesh(animation.m_FrameIndex, m_Cache[animation.m_FrameIndex]);

    // get and cache mesh to interpolate with
    if (!m_Cache[animation.m_InterpolationFrameIndex].size())
        m_pModel->GetMesh(animation.m_InterpolationFrameIndex,
                              m_Cache[animation.m_InterpolationFrameIndex]);

    // process interpolation and send resulting mesh to output
    return (QR_MD2::Interpolate(animation.m_InterpolationFactor,
                                m_Cache[animation.m_FrameIndex],
                                m_Cache[animation.m_InterpolationFrameIndex],
                                mesh) == QR_MD2Common::IE_C_Success);
}
//------------------------------------------------------------------------------
bool QR_MD2Animation::GetFrames(QR_SizeT start, QR_SizeT end, IFrames& frames) const
{
    // no model?
    if (!m_pModel)
        return false;

    // get linked model mesh count
    const QR_SizeT meshCount = m_pModel->GetMeshCount();

    // reserve memory for frames
    frames.reserve(meshCount);

    // iterate through meshes
    for (QR_SizeT i = start; i <= end; ++i)
    {
        // is index out of bounds?
        if (i >= meshCount)
            return false;

        // create mesh container
        std::auto_ptr<QR_Mesh> pMesh(new QR_Mesh());

        // get interpolation index
        QR_SizeT interpolationIndex = i + 1;

        // is interpolation index out of bounds?
        if (interpolationIndex > end)
            // set to start
            interpolationIndex = start;

        QR_Mesh frameMesh;
        QR_Mesh interpolationMesh;

        // get frame mesh and mesh to interpolate with
        m_pModel->GetMesh(i, frameMesh);
        m_pModel->GetMesh(interpolationIndex, interpolationMesh);

        // get vertex buffer count
        const QR_SizeT vbCount = frameMesh.size();

        // reserve memory for meshes
        pMesh->reserve(vbCount);

        // iterate through vertex buffers composing mesh
        for (QR_SizeT j = 0; j < vbCount; ++j)
        {
            // are frame compatibles?
            if (!frameMesh[j]->CompareFormat(*interpolationMesh[j]))
                return false;

            // not a 3D coordinate?
            if (frameMesh[j]->m_CoordType != QR_Vertex::IE_VC_XYZ)
                return false;

            // create and populate new interpolation vertex
            std::auto_ptr<QR_Vertex> pVertex(new QR_Vertex());
            pVertex->m_Type      = frameMesh[j]->m_Type;
            pVertex->m_Format    = frameMesh[j]->m_Format;
            pVertex->m_CoordType = frameMesh[j]->m_CoordType;

            // calculate final vertex stride (6 because interpolation vertex are
            // included too)
            pVertex->m_Stride = 6;

            // do include normals?
            if (pVertex->m_Format & QR_Vertex::IE_VF_Normals)
                // add normals (6 because interpolation normals are included too)
                pVertex->m_Stride += 6;

            // do include texture coordinates?
            if (pVertex->m_Format & QR_Vertex::IE_VF_TexCoords)
                pVertex->m_Stride += 2;

            // do include colors?
            if (pVertex->m_Format & QR_Vertex::IE_VF_Colors)
                pVertex->m_Stride += 4;

            // get vertex buffer data count
            const QR_SizeT bufferCount = frameMesh[j]->m_Buffer.size();

            // reserve size for final vertex buffer
            pVertex->m_Buffer.reserve((bufferCount / frameMesh[j]->m_Stride) * pVertex->m_Stride);

            // iterate through vertex buffer content
            for (QR_SizeT k = 0; k < bufferCount; k += frameMesh[j]->m_Stride)
            {
                QR_UInt32 index = 3;

                // set vertex and interpolation positions in destination buffer
                pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k]);
                pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + 1]);
                pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + 2]);
                pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k]);
                pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k + 1]);
                pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k + 2]);

                // do include normals?
                if (pVertex->m_Format & QR_Vertex::IE_VF_Normals)
                {
                    // set vertex and interpolation normals in destination buffer
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 1]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 2]);
                    pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k + index]);
                    pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k + index + 1]);
                    pVertex->m_Buffer.push_back(interpolationMesh[j]->m_Buffer[k + index + 2]);

                    index += 3;
                }

                // do include texture coordinates?
                if (pVertex->m_Format & QR_Vertex::IE_VF_TexCoords)
                {
                    // copy texture coordinates from source
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 1]);

                    index += 2;
                }

                // do include colors?
                if (pVertex->m_Format & QR_Vertex::IE_VF_Colors)
                {
                    // copy color from source
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 1]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 2]);
                    pVertex->m_Buffer.push_back(frameMesh[j]->m_Buffer[k + index + 3]);
                }
            }

            // add newly created mesh to mesh list
            pMesh->push_back(pVertex.get());
            pVertex.release();
        }

        // add newly created mesh to output list
        frames.push_back(pMesh.get());
        pMesh.release();

        // clear memory
        QR_STDTools::DelAndClear(frameMesh);
        QR_STDTools::DelAndClear(interpolationMesh);
    }

    return true;
}
//------------------------------------------------------------------------------
bool QR_MD2Animation::GetFrames(IFrameDictionary& frameDict) const
{
    // iterate through all registered animations
    for (IInfoDictionary::const_iterator it = m_InfoDictionary.begin(); it != m_InfoDictionary.end(); ++it)
    {
        // create new frame container
        std::auto_ptr<IFrames> pFrames(new IFrames());

        // calculate next frames for animation
        if (!GetFrames(it->second->m_Start, it->second->m_End, *pFrames.get()))
        {
            // get frames count
            const QR_SizeT frameCount = pFrames->size();

            // iterate through frames
            for (QR_SizeT i = 0; i < frameCount; ++i)
                // delete frame list content
                QR_STDTools::DelAndClear(*(*pFrames)[i]);

            // delete frame list
            QR_STDTools::DelAndClear(*pFrames);
            return false;
        }

        // add newly created frames to output dictionary
        frameDict[it->first] = pFrames.get();
        pFrames.release();
    }

    return true;
}
//------------------------------------------------------------------------------
