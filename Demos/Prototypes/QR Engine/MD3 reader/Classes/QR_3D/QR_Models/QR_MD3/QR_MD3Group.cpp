/******************************************************************************
 * ==> QR_MD3Group -----------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 model group, contains all items and functions needed to  *
 *               manage a complete MD3 model                                  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD3Group.h"

// std
#include <fstream>

// qr engine
#include "QR_STDTools.h"
#include "QR_FileTools.h"
#include "QR_Quaternion.h"
#include "QR_MD3Common.h"

//------------------------------------------------------------------------------
// QR_MD3Group::IInfo - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Group::IInfo::IInfo(bool createDefaultPrefixes) :
    m_ModelTemplate(L"%s"),
    m_SkinTemplate(L"%s"),
    m_AnimTemplate(L"%s")
{
    // do create default prefixes?
    if (createDefaultPrefixes)
    {
        m_Prefixes.push_back(L"lower");
        m_Prefixes.push_back(L"upper");
        m_Prefixes.push_back(L"head");
    }
}
//------------------------------------------------------------------------------
QR_MD3Group::IInfo::~IInfo()
{}
//------------------------------------------------------------------------------
// QR_MD3Group::IAnimationItem - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Group::IAnimationItem::IAnimationItem() :
    m_StartFrame(0),
    m_EndFrame(0),
    m_FramesPerSecond(0),
    m_Loop(false)
{}
//------------------------------------------------------------------------------
QR_MD3Group::IAnimationItem::IAnimationItem(QR_SizeT startFrame,
                                            QR_SizeT endFrame,
                                            QR_SizeT fps,
                                            bool     loop) :
    m_StartFrame(startFrame),
    m_EndFrame(endFrame),
    m_FramesPerSecond(fps),
    m_Loop(loop)
{}
//------------------------------------------------------------------------------
QR_MD3Group::IAnimationItem::~IAnimationItem()
{}
//------------------------------------------------------------------------------
// QR_MD3Group::ILink - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Group::ILink::ILink() :
    m_TagIndex(0),
    m_pItem(NULL)
{}
//------------------------------------------------------------------------------
QR_MD3Group::ILink::ILink(QR_SizeT tagIndex, IItem* pItem) :
    m_TagIndex(tagIndex),
    m_pItem(pItem)
{}
//------------------------------------------------------------------------------
QR_MD3Group::ILink::~ILink()
{}
//------------------------------------------------------------------------------
// QR_MD3Group::IItem - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Group::IItem::IItem() :
    m_pModel(NULL),
    m_Gesture(QR_MD3AnimCfgFile::IE_AG_MAX_ANIMATIONS)
{}
//------------------------------------------------------------------------------
QR_MD3Group::IItem::~IItem()
{
    // clear memory
    QR_STDTools::DelAndClear(m_Textures);
    QR_STDTools::DelAndClear(m_LinksFrom);
    QR_STDTools::DelAndClear(m_LinksTo);
    QR_STDTools::DelAndClear(m_Animations);

    // delete model
    if (m_pModel)
        delete m_pModel;
}
//------------------------------------------------------------------------------
// QR_MD3Group - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Group::QR_MD3Group() :
    QR_FramedModelGroup(),
    m_PrefixKeyword(L"%s")
{}
//------------------------------------------------------------------------------
QR_MD3Group::~QR_MD3Group()
{
    // clear memory
    QR_STDTools::DelAndClear(m_Items);
}
//------------------------------------------------------------------------------
void QR_MD3Group::Clear()
{
    // clear memory
    QR_STDTools::DelAndClear(m_Items);

    // empty values
    m_ItemDictionary.clear();
}
//------------------------------------------------------------------------------
bool QR_MD3Group::Load(const std::wstring& dir, const IInfo& info)
{
    Clear();

    // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
    // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
    // (except for optional files) begins with the same prefix, that is usually head, upper or lower
    const QR_SizeT subModelGroupCount = info.m_Prefixes.size();

    // iterate through sub-models to load
    for (QR_SizeT i = 0; i < subModelGroupCount; ++i)
    {
        // create and populate MD3 sub-model
        std::auto_ptr<QR_MD3> pMD3(new QR_MD3());
        pMD3->SetVertexFormat((QR_Vertex::IEFormat)(QR_Vertex::IE_VF_Colors    |
                                                    QR_Vertex::IE_VF_TexCoords |
                                                    QR_Vertex::IE_VF_Normals));
        pMD3->SetColor(QR_Color(255, 255, 255, 255));

        // build model name
        const std::wstring modelName = BuildName(info.m_ModelTemplate, info.m_Prefixes[i]);

        // build model file name
        std::wstring modelFileName = QR_FileTools::AppendDelimiter(dir) + modelName + L".md3";

        // model file exists?
        if (!QR_FileTools::FileExists(modelFileName))
        {
            Clear();
            return false;
        }

        // open it
        if (pMD3->Load(modelFileName) != QR_MD3Common::IE_C_Success)
        {
            Clear();
            return false;
        }

        // create and populate model item
        std::auto_ptr<IItem> pItem(new IItem());
        pItem->m_Name   = info.m_Prefixes[i];
        pItem->m_pModel = pMD3.get();
        pMD3.release();

        // build skin name
        const std::wstring skinName = BuildName(info.m_SkinTemplate, info.m_Prefixes[i]);

        // build skin file name
        std::wstring skinFileName = QR_FileTools::AppendDelimiter(dir) + skinName + L".skin";

        // open it
        if (!LoadSkin(skinFileName, pItem.get()))
        {
            Clear();
            return false;
        }

        // add item to list
        m_Items.push_back(pItem.get());
        m_ItemDictionary[modelName] = pItem.release();
    }

    // build animation file name
    const std::wstring animFileName = QR_FileTools::AppendDelimiter(dir)           +
                                      BuildName(info.m_AnimTemplate, L"animation") +
                                      L".cfg";

    // open it
    if (!LoadAnimations(info, animFileName))
    {
        Clear();
        return false;
    }

    LinkModel();

    return true;
}
//------------------------------------------------------------------------------
bool QR_MD3Group::Load(IMemoryDir& dir, const IInfo& info)
{
    Clear();

    // get sub-model group count. A sub-model group is usually composed of a model (.md3 file), a
    // skin (.skin file), and optional shader and texture files. All files belonging to sub-model
    // (except for optional files) begins with the same prefix, that is usually head, upper or lower
    const QR_SizeT subModelGroupCount = info.m_Prefixes.size();

    // iterate through sub-models to load
    for (QR_SizeT i = 0; i < subModelGroupCount; ++i)
    {
        // create and populate MD3 sub-model
        std::auto_ptr<QR_MD3> pMD3(new QR_MD3());
        pMD3->SetVertexFormat((QR_Vertex::IEFormat)(QR_Vertex::IE_VF_Colors    |
                                                    QR_Vertex::IE_VF_TexCoords |
                                                    QR_Vertex::IE_VF_Normals));
        pMD3->SetColor(QR_Color(255, 255, 255, 255));

        // build model name
        const std::wstring modelName = BuildName(info.m_ModelTemplate, info.m_Prefixes[i]);

        // build model file name
        std::wstring modelFileName = modelName + L".md3";

        // model file exists?
        if (!dir.FileExists(modelFileName))
        {
            Clear();
            return false;
        }

        // get buffer containing MD3 data
        QR_MemoryBuffer* pMD3Buffer = dir.GetFile(modelFileName);

        // found it?
        if (!pMD3Buffer)
        {
            Clear();
            return false;
        }

        // open it
        if (pMD3->Load(*pMD3Buffer, pMD3Buffer->GetSize()) != QR_MD3Common::IE_C_Success)
        {
            Clear();
            return false;
        }

        // create and populate model item
        std::auto_ptr<IItem> pItem(new IItem());
        pItem->m_Name   = info.m_Prefixes[i];
        pItem->m_pModel = pMD3.get();
        pMD3.release();

        // build skin name
        const std::wstring skinName = BuildName(info.m_SkinTemplate, info.m_Prefixes[i]);

        // build skin file name
        std::wstring skinFileName = skinName + L".skin";

        // get buffer containing skin data
        QR_MemoryBuffer* pSkinBuffer = dir.GetFile(skinFileName);

        // found it?
        if (!pSkinBuffer)
        {
            Clear();
            return false;
        }

        // open it
        if (!LoadSkin(*pSkinBuffer, pItem.get()))
        {
            Clear();
            return false;
        }

        // add item to list
        m_Items.push_back(pItem.get());
        m_ItemDictionary[modelName] = pItem.release();
    }

    // build animation file name
    const std::wstring animFileName = BuildName(info.m_AnimTemplate, L"animation") + L".cfg";

    // get buffer containing animation data
    QR_MemoryBuffer* pAnimCfgBuffer = dir.GetFile(animFileName);

    // found it?
    if (!pAnimCfgBuffer)
    {
        Clear();
        return false;
    }

    // open it
    if (!LoadAnimations(info, *pAnimCfgBuffer))
    {
        Clear();
        return false;
    }

    LinkModel();

    return true;
}
//------------------------------------------------------------------------------
bool QR_MD3Group::SetAnimation(const std::wstring& name, QR_MD3AnimCfgFile::IEGesture gesture)
{
    // get sub-model for which animation should be changed
    IItemDictionary::iterator it = m_ItemDictionary.find(name);

    // found it?
    if (it == m_ItemDictionary.end())
        return false;

    // get sub-model item
    IItem* pItem = it->second;

    // found it?
    if (!pItem)
        return false;

    // check if sub-model supports the gesture
    IAnimationDictionary::iterator itAnim = pItem->m_Animations.find(gesture);

    // is gesture supported by sub-model?
    if (itAnim == pItem->m_Animations.end())
        return false;

    // update loop value
    pItem->m_Animation.m_Loop = itAnim->second->m_Loop;

    // do loop animation?
    if (!itAnim->second->m_Loop)
    {
        pItem->m_Animation.m_FrameIndex              = itAnim->second->m_StartFrame;
        pItem->m_Animation.m_InterpolationFrameIndex = itAnim->second->m_StartFrame;
        pItem->m_Animation.m_InterpolationFactor     = 0.0;
    }

    // change sub-model gesture
    pItem->m_Gesture = gesture;
    return true;
}
//------------------------------------------------------------------------------
void QR_MD3Group::Draw(const QR_Double& elapsedTime) const
{
    // get sub-model item count
    const QR_SizeT itemCount = m_Items.size();

    // no sub-models to draw?
    if (!itemCount)
        return;

    // animate each sub-model
    for (QR_SizeT i = 0; i < itemCount; ++i)
        AnimateModel(elapsedTime, m_Items[i]);

    // get model matrix
    QR_Matrix16P modelMatrix = GetMatrix();

    // draw all sub-models, recursively
    DrawMesh(m_Items[0], modelMatrix);
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LoadSkin(const std::wstring& fileName, IItem* pItem)
{
    QR_MD3SkinFile skin;

    // load skin file
    if (!skin.Load(fileName))
        return false;

    return LinkSkin(skin, QR_FileTools::GetFileDir(fileName, true), pItem);
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LoadSkin(QR_Buffer& buffer, IItem* pItem)
{
    QR_MD3SkinFile skin;

    // load skin file
    if (!skin.Load(buffer, buffer.GetSize()))
        return false;

    return LinkSkin(skin, L"", pItem);
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LoadAnimations(const IInfo& info, const std::wstring& fileName)
{
    QR_MD3AnimCfgFile animations;

    // load animation configuration file
    if (!animations.Load(fileName))
        return false;

    return LinkAnimations(info, animations);
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LoadAnimations(const IInfo& info, QR_Buffer& buffer)
{
    QR_MD3AnimCfgFile animations;

    // load animation configuration file
    if (!animations.Load(buffer, buffer.GetSize()))
        return false;

    return LinkAnimations(info, animations);
}
//------------------------------------------------------------------------------
void QR_MD3Group::LinkModel()
{
    // get items count
    const QR_SizeT itemCount = m_Items.size();

    // iterate through items
    for (QR_SizeT i = 0; i < itemCount; ++i)
    {
        // item contains model?
        if (!m_Items[i]->m_pModel)
            continue;

        // get model parser
        const QR_MD3Parser* pParser = m_Items[i]->m_pModel->GetParser();

        // found it?
        if (!pParser)
            continue;

        // get model tag count
        const QR_SizeT tagCount = pParser->m_Header.m_TagCount;

        // iterate through model tags
        for (QR_SizeT j = 0; j < tagCount; ++j)
        {
            // get tag name to link with
            const std::string linkName = pParser->m_pTag[j].m_Name;

            // iterate throuugh remaining items
            for (QR_SizeT k = i + 1; k < itemCount; ++k)
            {
                // next item contains model
                if (!m_Items[k]->m_pModel)
                    continue;

                // get target model parser
                const QR_MD3Parser* pTargetParser = m_Items[k]->m_pModel->GetParser();

                // found it?
                if (!pTargetParser)
                    continue;

                // get target model tag count
                const QR_SizeT targetTagCount = pTargetParser->m_Header.m_TagCount;

                // iterate through target tags
                for (QR_SizeT l = 0; l < targetTagCount; ++l)
                {
                    // get target link name
                    const std::string targetName = pTargetParser->m_pTag[l].m_Name;

                    // found models to link?
                    if (linkName == targetName)
                    {
                        // build source and destination tag name to search
                        std::string srcTagName = BuildTagName(m_Items[i]->m_Name);
                        std::string dstTagName = BuildTagName(m_Items[k]->m_Name);

                        // set default source and destination tag indexes to link with
                        std::size_t srcIndex = j;
                        std::size_t dstIndex = l;

                        // search for source index by name. If not found, default index will be used
                        if (!srcTagName.empty())
                            for (std::size_t m = 0; m < tagCount; ++m)
                                if (dstTagName == pParser->m_pTag[m].m_Name)
                                {
                                    srcIndex = m;
                                    break;
                                }

                        // search for dest index by name. If not found, default index will be used
                        if (!dstTagName.empty())
                            for (std::size_t m = 0; m < targetTagCount; ++m)
                                if (srcTagName == pTargetParser->m_pTag[m].m_Name)
                                {
                                    dstIndex = m;
                                    break;
                                }

                        // link models
                        AddLink(m_Items[i]->m_LinksTo,   m_Items[k], srcIndex);
                        AddLink(m_Items[k]->m_LinksFrom, m_Items[i], dstIndex);
                        break;
                    }
                }
            }
        }
    }
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LinkSkin(const QR_MD3SkinFile& skin, const std::wstring& dir, IItem* pItem)
{
    // no item?
    if (!pItem)
        return false;

    // item contains no model?
    if (!pItem->m_pModel)
        return false;

    // get model parser
    const QR_MD3Parser* pParser = pItem->m_pModel->GetParser();

    // found it?
    if (!pParser)
        return false;

    // iterate through model meshes
    for (QR_SizeT i = 0; i < pParser->m_Header.m_MeshCount; ++i)
    {
        // get mesh name
        std::wstring meshName = QR_StringTools::StrToWStr(pParser->m_pMesh[i].m_Header.m_Name);

        // get texture path from skin file
        std::wstring texturePath = skin.GetPath(L"" + meshName);

        // search for texture name position in line
        QR_SizeT texturePos = texturePath.rfind(L'/');

        // found it?
        if (texturePos == texturePath.npos)
            continue;

        // create and populate new texture
        std::auto_ptr<ITexture> pTexture(new ITexture());
        pTexture->m_Dir  = dir;
        pTexture->m_Name = texturePath.substr(texturePos + 1, texturePath.length() - (texturePos + 1));

        // Load texture associated with skin
        if (m_fOnLoadTexture)
            if (!m_fOnLoadTexture(pTexture.get()))
                return false;

        // add texture to model item
        pItem->m_Textures.push_back(pTexture.get());
        pTexture.release();
    }

    return true;
}
//------------------------------------------------------------------------------
bool QR_MD3Group::LinkAnimations(const IInfo& info, const QR_MD3AnimCfgFile& animations)
{
    const QR_SizeT animCount  = animations.GetItemCount();
          QR_SizeT lowerDelta = 0;

    // iterate through animations to link
    for (QR_SizeT i = 0; i < animCount; ++i)
    {
        // get animation item
        QR_MD3AnimCfgFile::IItem* pItem = animations.GetItem(i);

        // found it?
        if (!pItem)
            continue;

        // search model part to which animation should be linked
        if (pItem->m_Gesture >= QR_MD3AnimCfgFile::IE_AG_BOTH_DEATH1 &&
            pItem->m_Gesture <= QR_MD3AnimCfgFile::IE_AG_BOTH_DEAD3)
        {
            // iterate through model parts
            for (QR_SizeT j = 0; j < info.m_Prefixes.size(); ++j)
                // found upper or lower index?
                if (info.m_Prefixes[j] == L"upper" || info.m_Prefixes[j] == L"lower")
                {
                    // create and populate new animation item
                    std::auto_ptr<IAnimationItem> pAnimItem(new IAnimationItem());
                    pAnimItem->m_StartFrame      = pItem->m_StartFrame;
                    pAnimItem->m_EndFrame        = pItem->m_StartFrame + (pItem->m_FrameCount - 1);
                    pAnimItem->m_Loop            = pItem->m_LoopingFrames;
                    pAnimItem->m_FramesPerSecond = pItem->m_FramesPerSecond;

                    // add animation to model item
                    m_Items[j]->m_Animations[pItem->m_Gesture] = pAnimItem.get();
                    pAnimItem.release();
                }
        }
        else
        if (pItem->m_Gesture >= QR_MD3AnimCfgFile::IE_AG_TORSO_GESTURE &&
            pItem->m_Gesture <= QR_MD3AnimCfgFile::IE_AG_TORSO_STAND2)
        {
            // iterate through model parts
            for (QR_SizeT j = 0; j < info.m_Prefixes.size(); ++j)
                // found upper index?
                if (info.m_Prefixes[j] == L"upper")
                {
                    // create and populate new animation item
                    std::auto_ptr<IAnimationItem> pAnimItem(new IAnimationItem());
                    pAnimItem->m_StartFrame      = pItem->m_StartFrame;
                    pAnimItem->m_EndFrame        = pItem->m_StartFrame + (pItem->m_FrameCount - 1);
                    pAnimItem->m_Loop            = pItem->m_LoopingFrames;
                    pAnimItem->m_FramesPerSecond = pItem->m_FramesPerSecond;

                    // add animation to model item
                    m_Items[j]->m_Animations[pItem->m_Gesture] = pAnimItem.get();
                    pAnimItem.release();
                }
        }
        else
        if (pItem->m_Gesture >= QR_MD3AnimCfgFile::IE_AG_LEGS_WALK_CROUCHING &&
            pItem->m_Gesture <= QR_MD3AnimCfgFile::IE_AG_LEGS_TURN)
        {
            // do calculate animation delta to apply to lower model part?
            if (!lowerDelta)
            {
                // get start and end animations from which delta should be calculated
                QR_MD3AnimCfgFile::IItem* pStartItem = animations.GetItem(QR_MD3AnimCfgFile::IE_AG_TORSO_GESTURE);
                QR_MD3AnimCfgFile::IItem* pEndItem   = animations.GetItem(QR_MD3AnimCfgFile::IE_AG_LEGS_WALK_CROUCHING);

                // found them?
                if (!pStartItem || !pEndItem)
                    return false;

                // calculate animation delta to apply to lower model part
                lowerDelta = pEndItem->m_StartFrame - pStartItem->m_StartFrame;
            }

            // iterate through model parts
            for (QR_SizeT j = 0; j < info.m_Prefixes.size(); ++j)
                // found lower index?
                if (info.m_Prefixes[j] == L"lower")
                {
                    // create and populate new animation item
                    std::auto_ptr<IAnimationItem> pAnimItem(new IAnimationItem());
                    pAnimItem->m_StartFrame      = pItem->m_StartFrame;
                    pAnimItem->m_EndFrame        = pItem->m_StartFrame + (pItem->m_FrameCount - 1);
                    pAnimItem->m_Loop            = pItem->m_LoopingFrames;
                    pAnimItem->m_FramesPerSecond = pItem->m_FramesPerSecond;

                    // apply delta to animation values
                    pAnimItem->m_StartFrame -= lowerDelta;
                    pAnimItem->m_EndFrame   -= lowerDelta;

                    // add animation to model item
                    m_Items[j]->m_Animations[pItem->m_Gesture] = pAnimItem.get();
                    pAnimItem.release();
                }
        }
    }

    return true;
}
//------------------------------------------------------------------------------
std::wstring QR_MD3Group::BuildName(const std::wstring& templateName, const std::wstring& prefix) const
{
    // no template to build from?
    if (templateName.empty())
        return prefix;

    // build name from template
    return QR_StringTools::SearchAndReplace(templateName, m_PrefixKeyword, prefix, false);
}
//------------------------------------------------------------------------------
std::string QR_MD3Group::BuildTagName(const std::wstring& itemName) const
{
    if (QR_StringTools::ToLowerCase(itemName) == L"head")
        return "tag_head";
    else
    if (QR_StringTools::ToLowerCase(itemName) == L"upper")
        return "tag_torso";
    else
        return "";
}
//------------------------------------------------------------------------------
void QR_MD3Group::AddLink(ILinks& links, IItem* pTarget, QR_SizeT tagIndex) const
{
    // get link count
    const QR_SizeT linkCount = links.size();

    // iterate through existing links
    for (QR_SizeT i = 0; i < linkCount; ++i)
        // link already added?
        if (links[i]->m_pItem == pTarget)
            return;

    std::auto_ptr<ILink> pLink(new ILink(tagIndex, pTarget));

    // add link
    links.push_back(pLink.get());
    pLink.release();
}
//------------------------------------------------------------------------------
void QR_MD3Group::AnimateModel(QR_Double elapsedTime, IItem* pItem) const
{
    // no model item to animate?
    if (!pItem)
        return;

    // no selected gesture?
    if (pItem->m_Gesture >= QR_MD3AnimCfgFile::IE_AG_MAX_ANIMATIONS)
        return;

    // model contains no animation?
    if (!pItem->m_Animations.size())
    {
        pItem->m_Gesture = QR_MD3AnimCfgFile::IE_AG_MAX_ANIMATIONS;
        return;
    }

    // get animation info
    IAnimationDictionary::iterator it = pItem->m_Animations.find(pItem->m_Gesture);

    // found it?
    if (it == pItem->m_Animations.end())
    {
        pItem->m_Gesture = QR_MD3AnimCfgFile::IE_AG_MAX_ANIMATIONS;
        return;
    }

    // extract animation info item
    IAnimationItem* pAnimItem = it->second;

    // found it?
    if (!pAnimItem)
    {
        pItem->m_Gesture = QR_MD3AnimCfgFile::IE_AG_MAX_ANIMATIONS;
        return;
    }

    // animate model
    pItem->m_Animation = GetAnimation(elapsedTime,
                                      pAnimItem->m_FramesPerSecond,
                                      pAnimItem->m_StartFrame,
                                      pAnimItem->m_EndFrame,
                                      pAnimItem->m_Loop,
                                      pItem->m_Animation.m_FrameIndex,
                                      pItem->m_Animation.m_InterpolationFactor);
}
//------------------------------------------------------------------------------
bool QR_MD3Group::GetMesh(const IItem* pItem, QR_Mesh& mesh, QR_Mesh& nextMesh) const
{
    // no item?
    if (!pItem)
        return false;

    // no model?
    if (!pItem->m_pModel)
        return false;

    // is animation valid?
    if (pItem->m_Animation.m_FrameIndex              == M_MD3_Invalid_Index ||
        pItem->m_Animation.m_InterpolationFrameIndex == M_MD3_Invalid_Index)
        return false;

    // get frame mesh and mesh to interpolate with
    pItem->m_pModel->GetMesh(pItem->m_Animation.m_FrameIndex,              mesh);
    pItem->m_pModel->GetMesh(pItem->m_Animation.m_InterpolationFrameIndex, nextMesh);

    return true;
}
//------------------------------------------------------------------------------
void QR_MD3Group::DrawMesh(IItem* pItem, const QR_Matrix16P& matrix) const
{
    // no sub-model item?
    if (!pItem)
        return;

    // no sub-model to draw?
    if (!pItem->m_pModel)
        return;

    // a draw item function is required in order to draw the sub-model
    if (m_fOnDrawItem)
    {
        QR_Mesh mesh;
        QR_Mesh nextMesh;

        try
        {
            // get mesh to draw
            if (!GetMesh(pItem, mesh, nextMesh))
                return;

            // draw sub-model item
            m_fOnDrawItem(pItem->m_pModel,
                          pItem->m_Textures,
                          matrix,
                          &mesh,
                          &nextMesh,
                          pItem->m_Animation.m_InterpolationFactor);
        }
        catch (...)
        {}

        // clear memory
        QR_STDTools::DelAndClear(mesh);
        QR_STDTools::DelAndClear(nextMesh);
    }

    // get model parser
    const QR_MD3Parser* pParser = pItem->m_pModel->GetParser();

    // found it?
    if (!pParser)
        return;

    // get interpolation factor
    const QR_Double interpolationFactor = pItem->m_Animation.m_InterpolationFactor;

    // get model tag count
    const QR_SizeT linkCount = pItem->m_LinksTo.size();

    // iterate through model tags
    for (QR_SizeT i = 0; i < linkCount; ++i)
    {
        // get sub-model link
        ILink* pLink = pItem->m_LinksTo[i];

        // found it?
        if (!pLink)
            continue;

        // get tag count (tags contain some important link info as e.g. position and rotation matrix)
        const QR_SizeT           tagCount = pParser->m_Header.m_TagCount;

        // get current frame tag
        const QR_SizeT           tagIndex = (pItem->m_Animation.m_FrameIndex * tagCount) + pLink->m_TagIndex;
        const QR_MD3Parser::ITag tag      = pParser->m_pTag[tagIndex];

        // get frame tag to interpolate with
        const QR_SizeT           nextTagIndex = (pItem->m_Animation.m_InterpolationFrameIndex * tagCount) + pLink->m_TagIndex;
        const QR_MD3Parser::ITag nextTag      = pParser->m_pTag[nextTagIndex];

        // get interpolated linked sub-model position
        const QR_Vector3DP pos(tag.m_Position[0] + (interpolationFactor * (nextTag.m_Position[0] - tag.m_Position[0])),
                               tag.m_Position[1] + (interpolationFactor * (nextTag.m_Position[1] - tag.m_Position[1])),
                               tag.m_Position[2] + (interpolationFactor * (nextTag.m_Position[2] - tag.m_Position[2])));

        // get linked sub-model frame matrix, convert it to a quaternion
        QR_QuaternionP quat(QR_Matrix16P(tag.m_Rotation[0][0], tag.m_Rotation[1][0], tag.m_Rotation[2][0], 0.0f,
                                         tag.m_Rotation[0][1], tag.m_Rotation[1][1], tag.m_Rotation[2][1], 0.0f,
                                         tag.m_Rotation[0][2], tag.m_Rotation[1][2], tag.m_Rotation[2][2], 0.0f,
                                         0.0f,                 0.0f,                 0.0f,                 1.0f));

        // get linked sub-model next frame matrix, convert it to a quaternion
        QR_QuaternionP nextQuat(QR_Matrix16P(nextTag.m_Rotation[0][0], nextTag.m_Rotation[1][0], nextTag.m_Rotation[2][0], 0.0f,
                                             nextTag.m_Rotation[0][1], nextTag.m_Rotation[1][1], nextTag.m_Rotation[2][1], 0.0f,
                                             nextTag.m_Rotation[0][2], nextTag.m_Rotation[1][2], nextTag.m_Rotation[2][2], 0.0f,
                                             0.0f,                     0.0f,                     0.0f,                     1.0f));

        // interpolate quaternions using spherical linear interpolation
        QR_QuaternionP interpolatedQuat = quat.Slerp(nextQuat,
                                                     interpolationFactor);

        // get resulting interpolated matrix
        QR_Matrix16P interpolatedMatrix = interpolatedQuat.GetMatrix();

        // set interpolated position inside resulting matrix directly
        interpolatedMatrix.m_Table[3][0] = pos.m_X;
        interpolatedMatrix.m_Table[3][1] = pos.m_Y;
        interpolatedMatrix.m_Table[3][2] = pos.m_Z;

        // draw next linked sub-model
        DrawMesh(pLink->m_pItem, interpolatedMatrix.Multiply(matrix));
    }
}
//------------------------------------------------------------------------------
