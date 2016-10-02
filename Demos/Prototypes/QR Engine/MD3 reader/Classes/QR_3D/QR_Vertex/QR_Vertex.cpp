/******************************************************************************
 * ==> QR_Vertex -------------------------------------------------------------*
 ******************************************************************************
 * Description : Vertex global enumeration and types                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Vertex.h"

// std
#include <memory>

// qr engine
#include "QR_Exception.h"

//------------------------------------------------------------------------------
// Class QR_Vertex - c++ cross-platform
//------------------------------------------------------------------------------
QR_Vertex::QR_Vertex() :
    m_Stride(0),
    m_Type(IE_VT_Unknown),
    m_Format(IE_VF_None),
    m_CoordType(IE_VC_XYZ)
{}
//------------------------------------------------------------------------------
QR_Vertex::~QR_Vertex()
{}
//------------------------------------------------------------------------------
QR_Vertex* QR_Vertex::Clone() const
{
    // clone vertex
    std::auto_ptr<QR_Vertex> pClone(new QR_Vertex());
    pClone->m_Stride    = m_Stride;
    pClone->m_Type      = m_Type;
    pClone->m_Format    = m_Format;
    pClone->m_CoordType = m_CoordType;
    return pClone.release();
}
//------------------------------------------------------------------------------
QR_SizeT QR_Vertex::CalculateStride() const
{
    QR_SizeT stride;

    // search for coordinate type
    switch (m_CoordType)
    {
        case IE_VC_XY:
            stride = 2;
            break;

        case IE_VC_XYZ:
            stride = 3;
            break;

        default:
            M_THROW_EXCEPTION("Unknown coordinate type");
    }

    // do include normals?
    if (m_Format & QR_Vertex::IE_VF_Normals)
        stride += 3;

    // do include texture coordinates?
    if (m_Format & QR_Vertex::IE_VF_TexCoords)
        stride += 2;

    // do include vertex color?
    if (m_Format & QR_Vertex::IE_VF_Colors)
        stride += 4;

    return stride;
}
//------------------------------------------------------------------------------
bool QR_Vertex::CompareFormat(const QR_Vertex& other) const
{
    return (m_Stride    == other.m_Stride &&
            m_Type      == other.m_Type   &&
            m_Format    == other.m_Format &&
            m_CoordType == other.m_CoordType);
}
//------------------------------------------------------------------------------
bool QR_Vertex::VerifyAlignment()
{
    #if defined(__BORLANDC__) || defined(__CODEGEARC__)
        #pragma warn -8008
        #pragma warn -8066
    #endif

    // check if vertex buffer data type is 4 bytes aligned (to avoid slow draw)
    return !(sizeof(M_Precision) % 4);

    #if defined(__BORLANDC__) || defined(__CODEGEARC__)
        #pragma warn .8066
        #pragma warn .8008
    #endif
}
//------------------------------------------------------------------------------
// Class QR_IndexBuffer - c++ cross-platform
//------------------------------------------------------------------------------
QR_IndexBuffer::QR_IndexBuffer() :
    m_pVertex(NULL)
{}
//------------------------------------------------------------------------------
QR_IndexBuffer::QR_IndexBuffer(QR_Vertex* pVertex) :
    m_pVertex(NULL)
{
    Populate(pVertex);
}
//------------------------------------------------------------------------------
QR_IndexBuffer::~QR_IndexBuffer()
{}
//------------------------------------------------------------------------------
void QR_IndexBuffer::Populate(QR_Vertex* pVertex)
{
    // no vertex descriptor?
    if (!pVertex)
        return;

    // is vertex buffer empty?
    if (pVertex->m_Buffer.size())
        return;

    // is stride defined and valid?
    if (pVertex->m_Stride < 2)
        return;

    // get vertices count
    const QR_SizeT count = pVertex->m_Buffer.size() / pVertex->m_Stride;

    // initialize memory for indexes
    m_Indexes.resize(count);

    // iterate through vertices to index
    for (QR_SizeT i = 0; i < count; ++i)
        // calculate and set vertex index
        m_Indexes[i] = i * pVertex->m_Stride;
}
//------------------------------------------------------------------------------
// Class QR_TexCoord - c++ cross-platform
//------------------------------------------------------------------------------
QR_TexCoord::QR_TexCoord() :
    m_U(0.0f),
    m_V(0.0f)
{}
//------------------------------------------------------------------------------
QR_TexCoord::QR_TexCoord(M_Precision u, M_Precision v) :
    m_U(u),
    m_V(v)
{}
//------------------------------------------------------------------------------
QR_TexCoord::~QR_TexCoord()
{}
//------------------------------------------------------------------------------
