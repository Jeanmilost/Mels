/******************************************************************************
 * ==> QR_MD3Parser ----------------------------------------------------------*
 ******************************************************************************
 * Description : Reads and exposes MD3 file content                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD3Parser.h"

// std
#include <memory>

#ifdef _WIN32
    // needed for some std::memset implementation, e.g. in CodeBlocks where
    // compiler will not found it otherwise
    #include <cstring>
#endif

// qr engine
#include "QR_Exception.h"
#include "QR_StdFileBuffer.h"
#include "QR_MD3Common.h"

//------------------------------------------------------------------------------
// Global defines
//------------------------------------------------------------------------------
#define M_MD3_Mesh_File_Version 15
#define M_MD3_ID                (('3' << 24) + ('P' << 16) + ('D' << 8) + 'I')
//------------------------------------------------------------------------------
// QR_MD3Parser::IHeader - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IHeader::IHeader() :
    QR_SealedBase<QR_MD3Parser::IHeader>(),
    m_ID(0),
    m_Version(0),
    m_Flags(0),
    m_FrameCount(0),
    m_TagCount(0),
    m_MeshCount(0),
    m_SkinCount(0),
    m_FrameOffset(0),
    m_TagStartOffset(0),
    m_TagEndOffset(0),
    m_FileSize(0)
{
    std::memset(m_FileName, 0x0, sizeof(m_FileName));
}
//------------------------------------------------------------------------------
QR_MD3Parser::IHeader::~IHeader()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::IHeader::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)   == 1);
        M_ASSERT(sizeof(QR_UInt32) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(&m_ID,             sizeof(QR_UInt32));
    buffer.Read(&m_Version,        sizeof(QR_UInt32));
    buffer.Read(m_FileName,        sizeof(m_FileName));
    buffer.Read(&m_Flags,          sizeof(QR_UInt32));
    buffer.Read(&m_FrameCount,     sizeof(QR_UInt32));
    buffer.Read(&m_TagCount,       sizeof(QR_UInt32));
    buffer.Read(&m_MeshCount,      sizeof(QR_UInt32));
    buffer.Read(&m_SkinCount,      sizeof(QR_UInt32));
    buffer.Read(&m_FrameOffset,    sizeof(QR_UInt32));
    buffer.Read(&m_TagStartOffset, sizeof(QR_UInt32));
    buffer.Read(&m_TagEndOffset,   sizeof(QR_UInt32));
    buffer.Read(&m_FileSize,       sizeof(QR_UInt32));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::IMeshInfo - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IMeshInfo::IMeshInfo() :
    QR_SealedBase<QR_MD3Parser::IMeshInfo>(),
    m_MeshID(0),
    m_Flags(0),
    m_MeshFrameCount(0),
    m_SkinCount(0),
    m_PolygonCount(0),
    m_FaceCount(0),
    m_FaceOffset(0),
    m_SkinOffset(0),
    m_UVOffset(0),
    m_PolygonOffset(0),
    m_MeshSize(0)
{
    std::memset(m_Name, 0x0, sizeof(m_Name));
}
//------------------------------------------------------------------------------
QR_MD3Parser::IMeshInfo::~IMeshInfo()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::IMeshInfo::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)   == 1);
        M_ASSERT(sizeof(QR_UInt32) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(&m_MeshID,         sizeof(QR_UInt32));
    buffer.Read(m_Name,            sizeof(m_Name));
    buffer.Read(&m_Flags,          sizeof(QR_UInt32));
    buffer.Read(&m_MeshFrameCount, sizeof(QR_UInt32));
    buffer.Read(&m_SkinCount,      sizeof(QR_UInt32));
    buffer.Read(&m_PolygonCount,   sizeof(QR_UInt32));
    buffer.Read(&m_FaceCount,      sizeof(QR_UInt32));
    buffer.Read(&m_FaceOffset,     sizeof(QR_UInt32));
    buffer.Read(&m_SkinOffset,     sizeof(QR_UInt32));
    buffer.Read(&m_UVOffset,       sizeof(QR_UInt32));
    buffer.Read(&m_PolygonOffset,  sizeof(QR_UInt32));
    buffer.Read(&m_MeshSize,       sizeof(QR_UInt32));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::ITag - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::ITag::ITag() : QR_SealedBase<QR_MD3Parser::ITag>()
{
    std::memset(m_Name,     0x0, sizeof(m_Name));
    std::memset(m_Position, 0x0, sizeof(m_Position));
    std::memset(m_Rotation, 0x0, sizeof(m_Rotation));
}
//------------------------------------------------------------------------------
QR_MD3Parser::ITag::~ITag()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::ITag::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)  == 1);
        M_ASSERT(sizeof(QR_Float) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_Name,     sizeof(m_Name));
    buffer.Read(m_Position, sizeof(m_Position));
    buffer.Read(m_Rotation, sizeof(m_Rotation));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::IBone - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IBone::IBone() :
    QR_SealedBase<QR_MD3Parser::IBone>(),
    m_Scale(0.0f)
{
    std::memset(m_Min,      0x0, sizeof(m_Min));
    std::memset(m_Max,      0x0, sizeof(m_Max));
    std::memset(m_Position, 0x0, sizeof(m_Position));
    std::memset(m_Creator,  0x0, sizeof(m_Creator));
}
//------------------------------------------------------------------------------
QR_MD3Parser::IBone::~IBone()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::IBone::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)  == 1);
        M_ASSERT(sizeof(QR_Float) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_Min,      sizeof(m_Min));
    buffer.Read(m_Max,      sizeof(m_Max));
    buffer.Read(m_Position, sizeof(m_Position));
    buffer.Read(&m_Scale,   sizeof(QR_Float));
    buffer.Read(m_Creator,  sizeof(m_Creator));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::IPolygon - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IPolygon::IPolygon() :
    QR_SealedBase<QR_MD3Parser::IPolygon>()
{
    std::memset(m_Vertex, 0x0, sizeof(m_Vertex));
    std::memset(m_Normal, 0x0, sizeof(m_Normal));
}
//------------------------------------------------------------------------------
QR_MD3Parser::IPolygon::~IPolygon()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::IPolygon::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)  == 1);
        M_ASSERT(sizeof(QR_Int16) == 2);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_Vertex, sizeof(m_Vertex));
    buffer.Read(m_Normal, sizeof(m_Normal));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::IFace - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IFace::IFace() :
    QR_SealedBase<QR_MD3Parser::IFace>()
{
    std::memset(m_VertexIndices, 0x0, sizeof(m_VertexIndices));
}
//------------------------------------------------------------------------------
QR_MD3Parser::IFace::~IFace()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::IFace::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_UInt32) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_VertexIndices, sizeof(m_VertexIndices));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::ITextureCoord - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::ITextureCoord::ITextureCoord() :
    QR_SealedBase<QR_MD3Parser::ITextureCoord>()
{
    std::memset(m_Coordinate, 0x0, sizeof(m_Coordinate));
}
//------------------------------------------------------------------------------
QR_MD3Parser::ITextureCoord::~ITextureCoord()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::ITextureCoord::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Float) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_Coordinate, sizeof(m_Coordinate));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::ISkin - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::ISkin::ISkin() :
    QR_SealedBase<QR_MD3Parser::ISkin>(),
    m_ShaderIndex(0)
{
    std::memset(m_Name, 0x0, sizeof(m_Name));
}
//------------------------------------------------------------------------------
QR_MD3Parser::ISkin::~ISkin()
{}
//------------------------------------------------------------------------------
void QR_MD3Parser::ISkin::Read(QR_Buffer& buffer)
{
    #ifdef _DEBUG
        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn -8008
            #pragma warn -8066
        #endif

        // cross-platform security, check type length
        M_ASSERT(sizeof(QR_Int8)   == 1);
        M_ASSERT(sizeof(QR_UInt32) == 4);

        #if defined(__BORLANDC__) || defined(__CODEGEARC__)
            #pragma warn .8066
            #pragma warn .8008
        #endif
    #endif

    // read data from file
    buffer.Read(m_Name,         sizeof(m_Name));
    buffer.Read(&m_ShaderIndex, sizeof(QR_UInt32));
}
//------------------------------------------------------------------------------
// QR_MD3Parser::IMesh - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::IMesh::IMesh() :
    QR_SealedBase<QR_MD3Parser::IMesh>(),
    m_pSkin(NULL),
    m_pTexCoord(NULL),
    m_pFace(NULL),
    m_pPolygon(NULL)
{}
//------------------------------------------------------------------------------
QR_MD3Parser::IMesh::~IMesh()
{
    // delete skins
    if (m_pSkin)
        delete[] m_pSkin;

    // delete texture coordinates
    if (m_pTexCoord)
        delete[] m_pTexCoord;

    // delete faces
    if (m_pFace)
        delete[] m_pFace;

    // delete polygons
    if (m_pPolygon)
        delete[] m_pPolygon;
}
//------------------------------------------------------------------------------
// QR_MD3Parser - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3Parser::QR_MD3Parser() :
    QR_ModelParser(),
    m_pBone(NULL),
    m_pTag(NULL),
    m_pMesh(NULL)
{}
//------------------------------------------------------------------------------
QR_MD3Parser::~QR_MD3Parser()
{
    // delete bones
    if (m_pBone)
        delete[] m_pBone;

    // delete tags
    if (m_pTag)
        delete[] m_pTag;

    // delete meshes
    if (m_pMesh)
        delete[] m_pMesh;
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3Parser::Load(const std::string& fileName)
{
    // create a file buffer and open it for read
    QR_StdFileBuffer buffer;
    buffer.Open(fileName, QR_FileBuffer::IE_M_Read);
    buffer.Seek(0, 0);

    // read MD3 content
    return Load(buffer, buffer.GetSize());
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3Parser::Load(const std::wstring& fileName)
{
    // create a file buffer and open it for read
    QR_StdFileBuffer buffer;
    buffer.Open(fileName, QR_FileBuffer::IE_M_Read);
    buffer.Seek(0, 0);

    // read MD3 content
    return Load(buffer, buffer.GetSize());
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3Parser::Load(QR_Buffer& buffer, const QR_Buffer::ISizeType& length)
{
    // is buffer empty?
    if (buffer.Empty())
        return QR_MD3Common::IE_C_EmptyBuffer;

    // read file header
    m_Header.Read(buffer);

    // is MD3 file and version correct?
    if ((m_Header.m_ID != M_MD3_ID) || ((QR_Float)m_Header.m_Version != M_MD3_Mesh_File_Version))
        return QR_MD3Common::IE_C_InvalidFileVersion;

    // create bones
    m_pBone = new IBone[m_Header.m_FrameCount];

    // read bones
    for (QR_UInt32 i = 0; i < m_Header.m_FrameCount; ++i)
        m_pBone[i].Read(buffer);

    // get tag count
    const QR_SizeT tagCount = m_Header.m_FrameCount * m_Header.m_TagCount;

    // create tags, for each animation there is a tag array
    m_pTag = new ITag[tagCount];

    // read tags
    for (QR_UInt32 i = 0; i < tagCount; ++i)
        m_pTag[i].Read(buffer);

    // create meshes
    m_pMesh = new IMesh[m_Header.m_MeshCount];

    // get current offset
    QR_BufferSizeType offset = buffer.GetOffset();

    // iterate through meshes to read
    for (QR_UInt32 i = 0; i < m_Header.m_MeshCount; ++i)
    {
        // go to next mesh
        buffer.Seek(0, offset);

        // read mesh header
        m_pMesh[i].m_Header.Read(buffer);

        // create mesh structure
        m_pMesh[i].m_pSkin     = new ISkin[m_pMesh[i].m_Header.m_SkinCount];
        m_pMesh[i].m_pFace     = new IFace[m_pMesh[i].m_Header.m_FaceCount];
        m_pMesh[i].m_pTexCoord = new ITextureCoord[m_pMesh[i].m_Header.m_PolygonCount];
        m_pMesh[i].m_pPolygon  =
                new IPolygon[m_pMesh[i].m_Header.m_PolygonCount * m_pMesh[i].m_Header.m_MeshFrameCount];

        // read skins
        for (QR_UInt32 j = 0; j < m_pMesh[i].m_Header.m_SkinCount; ++j)
            m_pMesh[i].m_pSkin[j].Read(buffer);

        // go to faces offset
        buffer.Seek(offset, m_pMesh[i].m_Header.m_FaceOffset);

        // read faces (also named triangles in many documentation)
        for (QR_UInt32 j = 0; j < m_pMesh[i].m_Header.m_FaceCount; ++j)
            m_pMesh[i].m_pFace[j].Read(buffer);

        // go to texture coords offset
        buffer.Seek(offset, m_pMesh[i].m_Header.m_UVOffset);

        // read texture coords
        for (QR_UInt32 j = 0; j < m_pMesh[i].m_Header.m_PolygonCount; ++j)
            m_pMesh[i].m_pTexCoord[j].Read(buffer);

        // go to polygons offset
        buffer.Seek(offset, m_pMesh[i].m_Header.m_PolygonOffset);

        // get polygon count
        const QR_SizeT polygonCount =
                m_pMesh[i].m_Header.m_MeshFrameCount * m_pMesh[i].m_Header.m_PolygonCount;

        // read polygons (also named vertices in many documentation)
        for (QR_UInt32 j = 0; j < polygonCount; ++j)
            m_pMesh[i].m_pPolygon[j].Read(buffer);

        // calculate next mesh offset
        offset += m_pMesh[i].m_Header.m_MeshSize;
    }

    return QR_MD3Common::IE_C_Success;
}
//------------------------------------------------------------------------------
