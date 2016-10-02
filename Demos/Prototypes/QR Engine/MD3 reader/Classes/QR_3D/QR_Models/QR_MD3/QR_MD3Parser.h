/******************************************************************************
 * ==> QR_MD3Parser ----------------------------------------------------------*
 ******************************************************************************
 * Description : Reads and exposes MD3 file content                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3ParserH
#define QR_MD3ParserH

// std
#include <cstdio>
#include <string>

// qr engine
#include "QR_Types.h"
#include "QR_Sealed.h"
#include "QR_ModelParser.h"
#include "QR_Buffer.h"

//------------------------------------------------------------------------------
// Global defines
//------------------------------------------------------------------------------
#define M_MD3_MAX_QPATH 64
//------------------------------------------------------------------------------

/**
* Reads and exposes MD3 file content
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3Parser : public QR_ModelParser
{
    public:
        /**
        * MD3 header
        *@note This class cannot be inherited
        */
        struct IHeader : QR_Sealed(IHeader)
        {
            QR_UInt32 m_ID;
            QR_UInt32 m_Version;
            QR_UInt8  m_FileName[M_MD3_MAX_QPATH];
            QR_UInt32 m_Flags;
            QR_UInt32 m_FrameCount;
            QR_UInt32 m_TagCount;
            QR_UInt32 m_MeshCount;
            QR_UInt32 m_SkinCount;
            QR_UInt32 m_FrameOffset;
            QR_UInt32 m_TagStartOffset;
            QR_UInt32 m_TagEndOffset;
            QR_UInt32 m_FileSize;

            IHeader();
            ~IHeader();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 mesh info
        *@note This class cannot be inherited
        */
        struct IMeshInfo : QR_Sealed(IMeshInfo)
        {
            QR_UInt32 m_MeshID;
            QR_Int8   m_Name[M_MD3_MAX_QPATH];
            QR_UInt32 m_Flags;
            QR_UInt32 m_MeshFrameCount;
            QR_UInt32 m_SkinCount;
            QR_UInt32 m_PolygonCount;
            QR_UInt32 m_FaceCount;
            QR_UInt32 m_FaceOffset;
            QR_UInt32 m_SkinOffset;
            QR_UInt32 m_UVOffset;
            QR_UInt32 m_PolygonOffset;
            QR_UInt32 m_MeshSize;

            IMeshInfo();
            ~IMeshInfo();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 tag structure, used to link, rotate and translate the children models
        *@note This class cannot be inherited
        */
        struct ITag : QR_Sealed(ITag)
        {
            QR_Int8  m_Name[M_MD3_MAX_QPATH];
            QR_Float m_Position[3];
            QR_Float m_Rotation[3][3];

            ITag();
            ~ITag();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 bone info
        *@note This class cannot be inherited
        */
        struct IBone : QR_Sealed(IBone)
        {
            QR_Float m_Min[3];
            QR_Float m_Max[3];
            QR_Float m_Position[3];
            QR_Float m_Scale;
            QR_Int8  m_Creator[16];

            IBone();
            ~IBone();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 polygon (also named vertex in documentation), contains the normals and vertex indices
        *@note This class cannot be inherited
        */
        struct IPolygon : QR_Sealed(IPolygon)
        {
            QR_Int16 m_Vertex[3];
            QR_Int8  m_Normal[2];

            IPolygon();
            ~IPolygon();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 face (also named triangle in documentation), contains the vertex indices and texture
        * coordinate arrays
        *@note This class cannot be inherited
        */
        struct IFace : QR_Sealed(IFace)
        {
            QR_UInt32 m_VertexIndices[3];

            IFace();
            ~IFace();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 texture UV coordinates
        *@note This class cannot be inherited
        */
        struct ITextureCoord : QR_Sealed(ITextureCoord)
        {
            QR_Float m_Coordinate[2];

            ITextureCoord();
            ~ITextureCoord();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 skin (i.e. texture name, also name shader in documentation)
        *@note This class cannot be inherited
        */
        struct ISkin : QR_Sealed(ISkin)
        {
            QR_Int8   m_Name[M_MD3_MAX_QPATH];
            QR_UInt32 m_ShaderIndex;

            ISkin();
            ~ISkin();

            /**
            * Reads data from file
            *@param buffer - buffer to read from
            */
            void Read(QR_Buffer& buffer);
        };

        /**
        * MD3 mesh
        *@note This class cannot be inherited
        */
        struct IMesh : QR_Sealed(IMesh)
        {
            IMeshInfo      m_Header;
            ISkin*         m_pSkin;
            ITextureCoord* m_pTexCoord;
            IFace*         m_pFace;
            IPolygon*      m_pPolygon;

            IMesh();
            ~IMesh();
        };

        IHeader m_Header;
        IBone*  m_pBone;
        ITag*   m_pTag;
        IMesh*  m_pMesh;

        QR_MD3Parser();
        virtual ~QR_MD3Parser();

        /**
        * Loads MD3 from file
        *@param fileName - file name
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        */
        virtual QR_Int32 Load(const std::string&  fileName);
        virtual QR_Int32 Load(const std::wstring& fileName);

        /**
        * Loads MD3 from buffer
        *@param buffer - buffer
        *@param length - length to read in buffer, in bytes (not used here, can be 0)
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        *@note Read will begin from current offset
        */
        virtual QR_Int32 Load(      QR_Buffer&            buffer,
                              const QR_Buffer::ISizeType& length);
};

#endif // QR_MD3ParserH
