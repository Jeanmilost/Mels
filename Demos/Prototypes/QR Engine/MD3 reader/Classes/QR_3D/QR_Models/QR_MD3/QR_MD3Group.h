/******************************************************************************
 * ==> QR_MD3Group -----------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 model group, contains all items and functions needed to  *
 *               manage a complete MD3 model                                  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3GroupH
#define QR_MD3GroupH

// std
#include <string>

// qr engine
#include "QR_Vector3D.h"
#include "QR_Matrix16.h"
#include "QR_StringTools.h"
#include "QR_MD3.h"
#include "QR_MD3SkinFile.h"
#include "QR_MD3AnimCfgFile.h"
#include "QR_FramedModelGroup.h"

/**
* MD3 model group, contains all items and functions needed to manage a complete MD3 model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3Group : public QR_FramedModelGroup
{
    public:
        /**
        * Group info, contains all informations to load group, as e.g. file templates, prefixes, ...
        */
        struct IInfo
        {
            std::wstring              m_ModelTemplate;
            std::wstring              m_SkinTemplate;
            std::wstring              m_AnimTemplate;
            QR_StringTools::IwStrings m_Prefixes;

            /**
            * Constructor
            *@param createDefaultPrefixes - if true, default prefixes will be created
            *@note First declared prefix always matchs with the root model
            *@note Be careful, the prefix order will influence on the manner the objects will be
            *      linked. For example, a head/upper/lower order will link the head to upper model,
            *      then the upper to lower model, whereas a lower/upper/head order will link the
            *      lower to upper model, then the upper to head model
            */
            IInfo(bool createDefaultPrefixes = true);

            virtual ~IInfo();
        };

        QR_MD3Group();
        virtual ~QR_MD3Group();

        /**
        * Clears group
        */
        virtual void Clear();

        /**
        * Loads group from dir
        *@param dir - dir containing all model files to load
        *@param info - group info
        *@return true on success, otherwise false
        */
        virtual bool Load(const std::wstring& dir, const IInfo& info);

        /**
        * Loads group from memory dir
        *@param dir - memory dir containing all model streams to load
        *@param info - group info
        *@return true on success, otherwise false
        */
        virtual bool Load(IMemoryDir& dir, const IInfo& info);

        /**
        * Selects animation to run for a sub-model
        *@param name - sub-model name
        *@param gesture - animation gesture to execute
        *@return true on success, otherwise false
        */
        virtual bool SetAnimation(const std::wstring& name, QR_MD3AnimCfgFile::IEGesture gesture);

        /**
        * Draws group
        *@param elapsedTime - elapsed time since last draw
        */
        virtual void Draw(const QR_Double& elapsedTime) const;

    protected:
        /**
        * Animation item, contains the information that defines a given animation
        */
        struct IAnimationItem
        {
            QR_SizeT m_StartFrame;
            QR_SizeT m_EndFrame;
            QR_SizeT m_FramesPerSecond;
            bool     m_Loop;

            IAnimationItem();

            /**
            * Constructor
            *@param startFrame - animation start frame index
            *@param endFrame - animation end frame index
            *@param fps - frame per seconds
            *@param loop - if true, animation should loop at end
            */
            IAnimationItem(QR_SizeT startFrame, QR_SizeT endFrame, QR_SizeT fps, bool loop);

            virtual ~IAnimationItem();
        };

        typedef std::map<QR_MD3AnimCfgFile::IEGesture, IAnimationItem*> IAnimationDictionary;

        struct IItem;

        /**
        * Link, it's a relationship between sub-models to animate
        */
        struct ILink
        {
            QR_SizeT m_TagIndex;
            IItem*   m_pItem;

            ILink();

            /**
            * Constructor
            *@param tagIndex - tag index in MD3 model from which link was built
            *@param pItem - item to link to
            */
            ILink(QR_SizeT tagIndex, IItem* pItem);

            virtual ~ILink();
        };

        typedef std::vector<ILink*> ILinks;

        /**
        * Group sub-model item, contains all info about a particular model belonging to group
        */
        struct IItem
        {
            std::wstring                 m_Name;
            QR_MD3*                      m_pModel;
            ITextures                    m_Textures;
            ILinks                       m_LinksFrom;
            ILinks                       m_LinksTo;
            QR_MD3AnimCfgFile::IEGesture m_Gesture;
            IAnimationDictionary         m_Animations;
            IAnimation                   m_Animation;

            IItem();
            virtual ~IItem();
        };

        typedef std::vector<IItem*>            IItems;
        typedef std::map<std::wstring, IItem*> IItemDictionary;

        std::wstring    m_PrefixKeyword;
        IItems          m_Items;
        IItemDictionary m_ItemDictionary;

        /**
        * Loads sub-model skin file
        *@param fileName - skin file name
        *@param pItem - item representing sub-model
        *@return true on success, otherwise false
        */
        virtual bool LoadSkin(const std::wstring& fileName, IItem* pItem);

        /**
        * Loads sub-model skin file
        *@param buffer - buffer containing skin data
        *@param pItem - item representing sub-model
        *@return true on success, otherwise false
        */
        virtual bool LoadSkin(QR_Buffer& buffer, IItem* pItem);

        /**
        * Loads animations from animation configuration file
        *@param info - model group info
        *@param fileName - animation config file name
        *@return true on success, otherwise false
        */
        virtual bool LoadAnimations(const IInfo& info, const std::wstring& fileName);

        /**
        * Loads animations from animation configuration file
        *@param info - model group info
        *@param buffer - buffer containing animation configuration
        *@return true on success, otherwise false
        */
        virtual bool LoadAnimations(const IInfo& info, QR_Buffer& buffer);

        /**
        * Links all models loaded or added in group together
        */
        virtual void LinkModel();

        /**
        * Links skin elements with model
        *@param skin - skin containing elements to load
        *@param pItem - item containing sub-model to link with skin
        *@return true on success, otherwise false
        */
        virtual bool LinkSkin(const QR_MD3SkinFile& skin, const std::wstring& dir, IItem* pItem);

        /**
        * Links animations with model
        *@param info - group info
        *@param animations - configuration file containing animations
        *@return true on success, otherwise false
        */
        virtual bool LinkAnimations(const IInfo& info, const QR_MD3AnimCfgFile& animations);

        /**
        * Builds name based on a template
        *@param templateName - template name to build from
        *@param prefix - prefix to use
        *@return built name
        */
        virtual std::wstring BuildName(const std::wstring& templateName,
                                       const std::wstring& prefix) const;

        /**
        * Builds tag name
        *@param itemName - item name
        *@return tag name, empty string if failed or on error
        */
        virtual std::string BuildTagName(const std::wstring& itemName) const;

        /**
        * Adds target model in links
        *@param links - links in which target should be added
        *@param pTarget - target to add to links
        *@param tagIndex - model tag index from which link was generated
        */
        virtual void AddLink(ILinks& links, IItem* pTarget, QR_SizeT tagIndex) const;

        /**
        * Animates model
        *@param elapsedTime - elapsed time since last rendering
        *@param pItem - model item containing sub-model to animate
        */
        virtual void AnimateModel(QR_Double elapsedTime, IItem* pItem) const;

        /**
        * Gets mesh to draw
        *@param pItem - sub-model item from which mesh should be get
        *@param[out] mesh - mesh to draw
        *@param[out] nextMesh - next mesh to interpolate with
        *@return true on success, otherwise false
        */
        virtual bool GetMesh(const IItem* pItem, QR_Mesh& mesh, QR_Mesh& nextMesh) const;

        /**
        * Draws the sub-model mesh
        *@param pItem - sub-model item to draw
        *@param matrix - sub-model matrix
        */
        virtual void DrawMesh(IItem* pItem, const QR_Matrix16P& matrix) const;
};

#endif
