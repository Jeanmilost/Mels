/******************************************************************************
 * ==> QR_MD3 ----------------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 model                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD3.h"

// std
#include <cstdio>
#include <cmath>

// qr engine
#include "QR_Exception.h"
#include "QR_STDTools.h"
#include "QR_StdFileBuffer.h"
#include "QR_MD3Common.h"

//------------------------------------------------------------------------------
// Global defines
//------------------------------------------------------------------------------
#define M_MD3_XYZ_Scale 64.0f // as shown in doc http://www.icculus.org/~phaethon/q3a/formats/md3format.html
//------------------------------------------------------------------------------
// QR_MD3 - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3::QR_MD3() :
    QR_FramedModel(),
    m_Color(QR_Color(255, 255, 255, 255))
{
    #ifdef _DEBUG
        // verify vertex buffer alignment
        if (!QR_Vertex::VerifyAlignment())
            M_THROW_EXCEPTION("Misaligned vertex buffer data type - must be 4 bytes aligned");
    #endif
}
//------------------------------------------------------------------------------
QR_MD3::~QR_MD3()
{
    QR_STDTools::DelAndClear(m_Vertices);
    QR_STDTools::DelAndClear(m_Normals);
    QR_STDTools::DelAndClear(m_TexCoords);
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3::Load(const std::string& fileName)
{
    const QR_Int32 result = m_Model.Load(fileName);
    PrepareMesh();
    return result;
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3::Load(const std::wstring& fileName)
{
    const QR_Int32 result = m_Model.Load(fileName);
    PrepareMesh();
    return result;
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3::Load(QR_Buffer& buffer, const QR_Buffer::ISizeType& length)
{
    const QR_Int32 result = m_Model.Load(buffer, length);
    PrepareMesh();
    return result;
}
//------------------------------------------------------------------------------
const QR_MD3Parser* QR_MD3::GetParser() const
{
    return &m_Model;
}
//------------------------------------------------------------------------------
void QR_MD3::SetColor(const QR_Color& color)
{
    m_Color = color;
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3::GetMesh(QR_SizeT index, QR_Mesh& mesh) const
{
    // is frame index out of bounds?
    if (index >= m_Model.m_Header.m_FrameCount)
        return QR_MD3Common::IE_C_IndexOutOfBounds;

    // basically stride is the coordinates values size
    QR_SizeT stride = 3;

    // do include normals?
    if (m_VertexFormat & QR_Vertex::IE_VF_Normals)
        stride += 3;

    // do include texture coordinates?
    if (m_VertexFormat & QR_Vertex::IE_VF_TexCoords)
        stride += 2;

    // do include colors?
    if (m_VertexFormat & QR_Vertex::IE_VF_Colors)
        stride += 4;

    // iterate through meshes to get
    for (QR_SizeT i = 0; i < m_Model.m_Header.m_MeshCount; ++i)
    {
        IVectors3D vertices;
        IVectors3D normals;
        ITexCoords texCoords;

        // get vertices from cache
        IVectorDictionary::const_iterator itVert = m_Vertices.find(i);

        // found them?
        if (itVert == m_Vertices.end())
            return QR_MD3Common::IE_C_VertexTableNotFound;

        // no vertex?
        if (itVert->second->empty())
            return QR_MD3Common::IE_C_VertexNotFound;

        // get normals from cache
        IVectorDictionary::const_iterator itNorm = m_Normals.find(i);

        // found them?
        if (itNorm == m_Normals.end())
            return QR_MD3Common::IE_C_NormalTableNotFound;

        // get texture coordinates from cache
        ITextureDictionary::const_iterator itText = m_TexCoords.find(i);

        // found them?
        if (itText == m_TexCoords.end())
            return QR_MD3Common::IE_C_TextureTableNotFound;

        // create and populate new vertex
        std::auto_ptr<QR_Vertex> pVertex(new QR_Vertex());
        pVertex->m_CoordType = QR_Vertex::IE_VC_XYZ;
        pVertex->m_Type      = QR_Vertex::IE_VT_Triangles;
        pVertex->m_Format    = m_VertexFormat;
        pVertex->m_Stride    = stride;

        // calculate indice count
        const QR_SizeT indiceCount = m_Model.m_pMesh[i].m_Header.m_FaceCount;

        // reserve memory for vertex buffer
        pVertex->m_Buffer.reserve(indiceCount * 3 * pVertex->m_Stride);

        // iterate through vertex indices and create final vertex buffer
        for (QR_SizeT j = 0; j < indiceCount; ++j)
            for (QR_UInt8 k = 0; k < 3; ++k)
                AddVertex((index * m_Model.m_pMesh[i].m_Header.m_PolygonCount),
                          m_Model.m_pMesh[i].m_pFace[j].m_VertexIndices[k],
                          m_VertexFormat,
                          *(itVert->second),
                          *(itNorm->second),
                          *(itText->second),
                          m_Color,
                          pVertex.get());

        // add mesh to output
        mesh.push_back(pVertex.get());
        pVertex.release();
    }

    return QR_MD3Common::IE_C_Success;
}
//------------------------------------------------------------------------------
QR_SizeT QR_MD3::GetMeshCount() const
{
    return m_Model.m_Header.m_FrameCount;
}
//------------------------------------------------------------------------------
QR_Int32 QR_MD3::Interpolate(const QR_Float& position, const QR_Mesh& mesh1,
        const QR_Mesh& mesh2, QR_Mesh& result)
{
    // get vertice count
    const QR_SizeT count = mesh1.size();

    // are mesh compatible?
    if (count != mesh2.size())
        return QR_MD3Common::IE_C_IncompatibleVertices;

    // iterate through mesh to interpolate
    for (QR_SizeT i = 0; i < count; ++i)
    {
        // are frame compatibles?
        if (!mesh1[i]->CompareFormat(*mesh2[i]))
            return QR_MD3Common::IE_C_IncompatibleFrames;

        // not a 3D coordinate?
        if (mesh1[i]->m_CoordType != QR_Vertex::IE_VC_XYZ)
            return QR_MD3Common::IE_C_Not3DCoords;

        // create and populate new interpolation vertex
        std::auto_ptr<QR_Vertex> pVertex(new QR_Vertex());
        pVertex->m_Stride    = mesh1[i]->m_Stride;
        pVertex->m_Type      = mesh1[i]->m_Type;
        pVertex->m_Format    = mesh1[i]->m_Format;
        pVertex->m_CoordType = mesh1[i]->m_CoordType;

        // get vertex buffer data count
        const QR_SizeT bufferCount = mesh1[i]->m_Buffer.size();

        pVertex->m_Buffer.reserve(bufferCount);

        // iterate through vertex buffer content
        for (QR_SizeT j = 0; j < bufferCount; j += mesh1[i]->m_Stride)
        {
            QR_UInt32 index = 3;

            // get positions
            QR_Vector3DP srcVec(mesh1[i]->m_Buffer[j],
                                mesh1[i]->m_Buffer[j + 1],
                                mesh1[i]->m_Buffer[j + 2]);
            QR_Vector3DP dstVec(mesh2[i]->m_Buffer[j],
                                mesh2[i]->m_Buffer[j + 1],
                                mesh2[i]->m_Buffer[j + 2]);

            // interpolate positions
            QR_Vector3DP vec = srcVec.Interpolate(dstVec, position);

            // set interpolated positions in destination buffer
            pVertex->m_Buffer.push_back(vec.m_X);
            pVertex->m_Buffer.push_back(vec.m_Y);
            pVertex->m_Buffer.push_back(vec.m_Z);

            // do include normals?
            if (mesh1[i]->m_Format & QR_Vertex::IE_VF_Normals)
            {
                // get normals
                QR_Vector3DP srcNormal(mesh1[i]->m_Buffer[j + index],
                                       mesh1[i]->m_Buffer[j + index + 1],
                                       mesh1[i]->m_Buffer[j + index + 2]);
                QR_Vector3DP dstNormal(mesh2[i]->m_Buffer[j + index],
                                       mesh2[i]->m_Buffer[j + index + 1],
                                       mesh2[i]->m_Buffer[j + index + 2]);

                // interpolate normals
                QR_Vector3DP normal = srcNormal.Interpolate(dstNormal, position);

                // set interpolated normals in destination buffer
                pVertex->m_Buffer.push_back(normal.m_X);
                pVertex->m_Buffer.push_back(normal.m_Y);
                pVertex->m_Buffer.push_back(normal.m_Z);

                index += 3;
            }

            // do include texture coordinates?
            if (mesh1[i]->m_Format & QR_Vertex::IE_VF_TexCoords)
            {
                // copy texture coordinates from source
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index]);
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index + 1]);

                index += 2;
            }

            // do include colors?
            if (mesh1[i]->m_Format & QR_Vertex::IE_VF_Colors)
            {
                // copy color from source
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index]);
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index + 1]);
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index + 2]);
                pVertex->m_Buffer.push_back(mesh1[i]->m_Buffer[j + index + 3]);
            }
        }

        // add interpolated mesh to output list
        result.push_back(pVertex.get());
        pVertex.release();
    }

    return QR_MD3Common::IE_C_Success;
}
//------------------------------------------------------------------------------
void QR_MD3::PrepareMesh()
{
    QR_STDTools::DelAndClear(m_Vertices);
    QR_STDTools::DelAndClear(m_Normals);
    QR_STDTools::DelAndClear(m_TexCoords);

    // iterate through meshes to get
    for (QR_SizeT i = 0; i < m_Model.m_Header.m_MeshCount; ++i)
    {
        // get mesh vertex count
        const QR_SizeT vertexCount =
                m_Model.m_pMesh[i].m_Header.m_PolygonCount * m_Model.m_pMesh[i].m_Header.m_MeshFrameCount;

        std::auto_ptr<IVectors3D> pVertices(new IVectors3D());
        std::auto_ptr<IVectors3D> pNormals(new IVectors3D());

        // reserve memory for temporary vertices to get from md3 file
        pVertices->resize(vertexCount);

        // reserve memory for temporary normals to get from md3 file
        pNormals->resize(vertexCount);

        // iterate through vertices to get
        for (QR_SizeT j = 0; j < vertexCount; ++j)
        {
            // uncompress vertex and add it to temporary vertex list
            (*pVertices)[j].m_X = m_Model.m_pMesh[i].m_pPolygon[j].m_Vertex[0] / M_MD3_XYZ_Scale;
            (*pVertices)[j].m_Y = m_Model.m_pMesh[i].m_pPolygon[j].m_Vertex[1] / M_MD3_XYZ_Scale;
            (*pVertices)[j].m_Z = m_Model.m_pMesh[i].m_pPolygon[j].m_Vertex[2] / M_MD3_XYZ_Scale;

            // uncompress normal and add it to temporary normal list
            (*pNormals)[j] = UncompressNormal(m_Model.m_pMesh[i].m_pPolygon[j].m_Normal[0],
                                              m_Model.m_pMesh[i].m_pPolygon[j].m_Normal[1]);
        }

        m_Vertices[i] = pVertices.get();
        pVertices.release();

        m_Normals[i] = pNormals.get();
        pNormals.release();

        std::auto_ptr<ITexCoords> pTexCoords(new ITexCoords());

        // get texture coordinates count
        const QR_SizeT texCoordCount = m_Model.m_pMesh[i].m_Header.m_PolygonCount;

        // reserve memory for temporary texture coordinates to get from md3 file
        pTexCoords->resize(texCoordCount);

        // iterate through texture coordinates to get
        for (QR_SizeT j = 0; j < texCoordCount; ++j)
        {
            // uncompress texture coordinates and add them to temporary texure coord list
            (*pTexCoords)[j].m_U = m_Model.m_pMesh[i].m_pTexCoord[j].m_Coordinate[0];
            (*pTexCoords)[j].m_V = m_Model.m_pMesh[i].m_pTexCoord[j].m_Coordinate[1];
        }

        m_TexCoords[i] = pTexCoords.get();
        pTexCoords.release();
    }
}
//------------------------------------------------------------------------------
void QR_MD3::AddVertex(      QR_SizeT            index,
                             QR_SizeT            indice,
                             QR_Vertex::IEFormat vertexFormat,
                       const IVectors3D&         vertices,
                       const IVectors3D&         normals,
                       const ITexCoords&         texCoords,
                       const QR_Color&           color,
                             QR_Vertex*          pVertex) const
{
    if (!pVertex)
        M_THROW_EXCEPTION("No output vertex");

    const QR_SizeT frameIndex = index + indice;

    // is indice out of bounds?
    if (frameIndex >= vertices.size())
        M_THROW_EXCEPTION("Vertice indice is out of bounds");

    pVertex->m_Buffer.push_back(vertices[frameIndex].m_X);
    pVertex->m_Buffer.push_back(vertices[frameIndex].m_Y);
    pVertex->m_Buffer.push_back(vertices[frameIndex].m_Z);

    // do include normals?
    if (vertexFormat & QR_Vertex::IE_VF_Normals)
    {
        // is indice out of bounds?
        if (frameIndex >= normals.size())
            M_THROW_EXCEPTION("Vertice indice is out of bounds");

        pVertex->m_Buffer.push_back(normals[frameIndex].m_X);
        pVertex->m_Buffer.push_back(normals[frameIndex].m_Y);
        pVertex->m_Buffer.push_back(normals[frameIndex].m_Z);
    }

    // do include texture coordinates?
    if (vertexFormat & QR_Vertex::IE_VF_TexCoords)
    {
        // is indice out of bounds?
        if (indice >= texCoords.size())
            M_THROW_EXCEPTION("Vertice indice is out of bounds");

        pVertex->m_Buffer.push_back(texCoords[indice].m_U);
        pVertex->m_Buffer.push_back(texCoords[indice].m_V);
    }

    // do include colors?
    if (vertexFormat & QR_Vertex::IE_VF_Colors)
    {
        pVertex->m_Buffer.push_back(color.GetRedF());
        pVertex->m_Buffer.push_back(color.GetGreenF());
        pVertex->m_Buffer.push_back(color.GetBlueF());
        pVertex->m_Buffer.push_back(color.GetAlphaF());
    }
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_MD3::UncompressNormal(QR_Int8 latitude, QR_Int8 longitude) const
{
    // calculate uncompression ratio
    const QR_Float ratio = (M_PI / 128.0f);

    // apply ratio to latitude and longitude
    const QR_Float lat = latitude  * ratio;
    const QR_Float lng = longitude * ratio;

    // calculate and return normal
    return QR_Vector3DP(std::cosf(lat) * std::sinf(lng),
                        std::sinf(lat) * std::sinf(lng),
                        std::cosf(lng));
}
//------------------------------------------------------------------------------
