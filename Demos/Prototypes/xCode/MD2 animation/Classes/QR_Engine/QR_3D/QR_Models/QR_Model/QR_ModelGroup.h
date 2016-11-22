/******************************************************************************
 * ==> QR_ModelGroup ---------------------------------------------------------*
 ******************************************************************************
 * Description : Model group, contains all items and functions needed to      *
 *               manage a complete model                                      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_ModelGroupH
#define QR_ModelGroupH

// std
#include <map>

// qr engine
#include "QR_Vector3D.h"
#include "QR_Matrix16.h"
#include "QR_MemoryBuffer.h"

/**
* Model group, contains all items and functions needed to manage a complete model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_ModelGroup
{
    public:
        /**
        * Matrix combination type enumeration
        */
        enum IECombinationType
        {
            IE_CT_Scale_Rotate_Translate,
            IE_CT_Scale_Translate_Rotate,
            IE_CT_Rotate_Translate_Scale,
            IE_CT_Rotate_Scale_Translate,
            IE_CT_Translate_Rotate_Scale,
            IE_CT_Translate_Scale_Rotate,
        };

        /**
        * Texture wrap mode enumeration
        */
        enum IEWrapMode
        {
            IE_WM_CLAMP_TO_EDGE,
            IE_WM_CLAMP_TO_BORDER,
            IE_WM_MIRRORED_REPEAT,
            IE_WM_REPEAT,
            IE_WM_MIRROR_CLAMP_TO_EDGE,
        };

        /**
        * Texture info, contains all info about a sub-model texture item
        */
        struct ITexture
        {
            QR_SizeT     m_Index;
            std::wstring m_Dir;
            std::wstring m_Name;
            IEWrapMode   m_WrapMode;

            ITexture();
            virtual ~ITexture();
        };

        typedef std::map<std::wstring, QR_MemoryBuffer*> IMemoryFileDictionary;

        /**
        * Memory directory, allows to create a structure closest to a dir that contains all files as
        * memory buffers
        */
        class IMemoryDir
        {
            public:
                /**
                * Constructor
                *@param deleteOnDestroy - if true, added memory streams will be deleted while dir
                *                         will be destroyed
                */
                IMemoryDir(bool deleteOnDestroy = false);

                virtual ~IMemoryDir();

                /**
                * Adds memory file
                *@param fileName - memory file name
                *@param pBuffer - memory buffer containing file data
                *@param overwrite - if true, existing file will be overwritten
                *@return true on success, otherwise false
                */
                virtual bool AddFile(const std::wstring&    fileName,
                                           QR_MemoryBuffer* pBuffer,
                                           bool             overwrite);

                /**
                * Gets file
                *@param fileName - memory file name to get
                *@return memory buffer containing file data, NULL if not found or on error
                */
                virtual QR_MemoryBuffer* GetFile(const std::wstring& fileName);

                /**
                * Checks if file exists
                *@param fileName - file name to check
                *@return true if file exists, otherwise false
                */
                virtual bool FileExists(const std::wstring& fileName) const;

            protected:
                IMemoryFileDictionary m_Files;
                bool                  m_DeleteOnDestroy;
        };

        typedef std::vector<ITexture*> ITextures;

        /**
        * Called when texture should be loaded
        *@param[in, out] pTexture - texture info, contains loaded index when function ends
        *@return true on success, otherwise false
        */
        #ifdef __CODEGEARC__
            typedef bool (__closure *ITfOnLoadTexture)(ITexture* pTexture);
        #else
            typedef bool (*ITfOnLoadTexture)(ITexture* pTexture);
        #endif

        QR_ModelGroup();
        virtual ~QR_ModelGroup();

        /**
        * Clears group
        */
        virtual void Clear() = 0;

        /**
        * Draws group
        *@param elapsedTime - elapsed time since last draw
        */
        virtual void Draw(const QR_Double& elapsedTime) const = 0;

        /**
        * Gets translation
        *@return Translation
        */
        virtual QR_Vector3DP GetTranslation() const;

        /**
        * Sets translation
        *@param Translation - Translation
        */
        virtual void SetTranslation(const QR_Vector3DP& translation);

        /**
        * Gets rotation on X axis
        *@return rotation angle on X axis in radians
        */
        virtual M_Precision GetRotationX() const;

        /**
        * Sets rotation on X axis
        *@param angle - rotation angle in radians
        */
        virtual void SetRotationX(const M_Precision& angle);

        /**
        * Gets rotation on Y axis
        *@return rotation angle on Y axis in radians
        */
        virtual M_Precision GetRotationY() const;

        /**
        * Sets rotation on Y axis
        *@param angle - rotation angle in radians
        */
        virtual void SetRotationY(const M_Precision& angle);

        /**
        * Gets rotation on Z axis
        *@return rotation angle on Z axis in radians
        */
        virtual M_Precision GetRotationZ() const;

        /**
        * Sets rotation on Z axis
        *@param angle - rotation angle in radians
        */
        virtual void SetRotationZ(const M_Precision& angle);

        /**
        * Gets scaling
        *@return scaling
        */
        virtual QR_Vector3DP GetScaling() const;

        /**
        * Sets scaling
        *@param scaling - scaling
        */
        virtual void SetScaling(const QR_Vector3DP& scaling);

        /**
        * Gets model matrix combination type
        *@return matrix combination type
        */
        virtual IECombinationType GetMatrixCombinationType() const;

        /**
        * Sets model matrix combination type
        *@param type - matrix combination type
        */
        virtual void SetMatrixCombinationType(IECombinationType type);

        /**
        * Gets initial model matrix
        *@return matrix
        */
        virtual QR_Matrix16P* GetInitialMatrix() const;

        /**
        * Sets initial model matrix
        *@param matrix - matrix
        */
        virtual void SetInitialMatrix(const QR_Matrix16P* pMatrix);

        /**
        * Sets OnLoadTexture callback
        *@param fHandler - function handler
        */
        virtual void Set_OnLoadTexture(ITfOnLoadTexture fHandler);

    protected:
        /**
        * Rotation data
        */
        struct IRotation
        {
            M_Precision  m_Angle;
            QR_Vector3DP m_Axis;

            /**
            * Constructor
            */
            IRotation();

            /**
            * Constructor
            *@param angle - angle
            *@param axis - rotation axis
            */
            IRotation(const M_Precision& angle, const QR_Vector3DP& axis);

            /**
            * Destructor
            */
            virtual ~IRotation();
        };

        QR_Vector3DP      m_Scaling;
        QR_Vector3DP      m_Translation;
        IRotation         m_RotationX;
        IRotation         m_RotationY;
        IRotation         m_RotationZ;
        IECombinationType m_CombinationType;
        QR_Matrix16P*     m_pInitialMatrix;
        ITfOnLoadTexture  m_fOnLoadTexture;

        /**
        * Gets model matrix
        *@return matrix
        */
        virtual QR_Matrix16P GetMatrix() const;
};

#endif
