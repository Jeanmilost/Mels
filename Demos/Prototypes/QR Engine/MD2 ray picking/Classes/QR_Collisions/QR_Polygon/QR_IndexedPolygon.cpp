/******************************************************************************
 * ==> QR_IndexedPolygon -----------------------------------------------------*
 ******************************************************************************
 * Description : Polygon connected to a vertex buffer and that can extract    *
 *               data from it directly                                        *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_IndexedPolygon.h"

// qr engine
#include "QR_Exception.h"
#include "QR_SimplePolygon.h"

//------------------------------------------------------------------------------
// QR_IndexedPolygon - c++ cross-platform
//------------------------------------------------------------------------------
QR_IndexedPolygon::QR_IndexedPolygon() :
    QR_Polygon(),
    m_pVertexBuffer(NULL)
{
    std::memset(&m_Index, 0x0, sizeof(QR_SizeT) * 3);
}
//------------------------------------------------------------------------------
QR_IndexedPolygon::QR_IndexedPolygon(      QR_SizeT            v1i,
                                           QR_SizeT            v2i,
                                           QR_SizeT            v3i,
                                     const QR_Vertex::IBuffer* pVertexBuffer) :
    QR_Polygon(),
    m_pVertexBuffer(pVertexBuffer)
{
    m_Index[0] = v1i;
    m_Index[1] = v2i;
    m_Index[2] = v3i;
}
//------------------------------------------------------------------------------
QR_IndexedPolygon::~QR_IndexedPolygon()
{}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::SetVertexInfo(      QR_SizeT            v1i,
                                            QR_SizeT            v2i,
                                            QR_SizeT            v3i,
                                      const QR_Vertex::IBuffer* pVertexBuffer)
{
    m_Index[0]      = v1i;
    m_Index[1]      = v2i;
    m_Index[2]      = v3i;
    m_pVertexBuffer = pVertexBuffer;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_IndexedPolygon::GetVertex(QR_UInt8 index) const
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    // search for index to get
    switch (index)
    {
        case 0:
            return QR_Vector3DP((*m_pVertexBuffer)[m_Index[0]],
                                (*m_pVertexBuffer)[m_Index[0] + 1],
                                (*m_pVertexBuffer)[m_Index[0] + 2]);

        case 1:
            return QR_Vector3DP((*m_pVertexBuffer)[m_Index[1]],
                                (*m_pVertexBuffer)[m_Index[1] + 1],
                                (*m_pVertexBuffer)[m_Index[1] + 2]);

        case 2:
            return QR_Vector3DP((*m_pVertexBuffer)[m_Index[2]],
                                (*m_pVertexBuffer)[m_Index[2] + 1],
                                (*m_pVertexBuffer)[m_Index[2] + 2]);

        default:
            return QR_Vector3DP();
    }
}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::SetVertex(QR_UInt8 index, const QR_Vector3DP& vertex)
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    // make vertex buffer accessible again
    QR_Vertex::IBuffer* pBuffer = const_cast<QR_Vertex::IBuffer*>(m_pVertexBuffer);
    M_ASSERT(pBuffer);

    // search for index to get
    switch (index)
    {
        case 0:
            (*pBuffer)[m_Index[0]]     = vertex.m_X;
            (*pBuffer)[m_Index[0] + 1] = vertex.m_Y;
            (*pBuffer)[m_Index[0] + 2] = vertex.m_Z;
            return;

        case 1:
            (*pBuffer)[m_Index[1]]     = vertex.m_X;
            (*pBuffer)[m_Index[1] + 1] = vertex.m_Y;
            (*pBuffer)[m_Index[1] + 2] = vertex.m_Z;
            return;

        case 2:
            (*pBuffer)[m_Index[2]]     = vertex.m_X;
            (*pBuffer)[m_Index[2] + 1] = vertex.m_Y;
            (*pBuffer)[m_Index[2] + 2] = vertex.m_Z;
            return;
    }
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_IndexedPolygon::GetVertex1() const
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    return QR_Vector3DP((*m_pVertexBuffer)[m_Index[0]],
                        (*m_pVertexBuffer)[m_Index[0] + 1],
                        (*m_pVertexBuffer)[m_Index[0] + 2]);
}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::SetVertex1(const QR_Vector3DP& value)
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    // make vertex buffer accessible again
    QR_Vertex::IBuffer* pBuffer = const_cast<QR_Vertex::IBuffer*>(m_pVertexBuffer);
    M_ASSERT(pBuffer);

    (*pBuffer)[m_Index[0]]     = value.m_X;
    (*pBuffer)[m_Index[0] + 1] = value.m_Y;
    (*pBuffer)[m_Index[0] + 2] = value.m_Z;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_IndexedPolygon::GetVertex2() const
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    return QR_Vector3DP((*m_pVertexBuffer)[m_Index[1]],
                        (*m_pVertexBuffer)[m_Index[1] + 1],
                        (*m_pVertexBuffer)[m_Index[1] + 2]);
}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::SetVertex2(const QR_Vector3DP& value)
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    // make vertex buffer accessible again
    QR_Vertex::IBuffer* pBuffer = const_cast<QR_Vertex::IBuffer*>(m_pVertexBuffer);
    M_ASSERT(pBuffer);

    (*pBuffer)[m_Index[1]]     = value.m_X;
    (*pBuffer)[m_Index[1] + 1] = value.m_Y;
    (*pBuffer)[m_Index[1] + 2] = value.m_Z;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_IndexedPolygon::GetVertex3() const
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    return QR_Vector3DP((*m_pVertexBuffer)[m_Index[2]],
                        (*m_pVertexBuffer)[m_Index[2] + 1],
                        (*m_pVertexBuffer)[m_Index[2] + 2]);
}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::SetVertex3(const QR_Vector3DP& value)
{
    M_ASSERT(m_pVertexBuffer);
    M_ASSERT(m_pVertexBuffer->size());

    // make vertex buffer accessible again
    QR_Vertex::IBuffer* pBuffer = const_cast<QR_Vertex::IBuffer*>(m_pVertexBuffer);
    M_ASSERT(pBuffer);

    (*pBuffer)[m_Index[2]]     = value.m_X;
    (*pBuffer)[m_Index[2] + 1] = value.m_Y;
    (*pBuffer)[m_Index[2] + 2] = value.m_Z;
}
//------------------------------------------------------------------------------
QR_Polygon* QR_IndexedPolygon::GetClone() const
{
    // copies the polygon, then returns the copy
    return new QR_IndexedPolygon(m_Index[0], m_Index[1], m_Index[2], m_pVertexBuffer);
}
//------------------------------------------------------------------------------
void QR_IndexedPolygon::GetData(      QR_SizeT&            i1,
                                      QR_SizeT&            i2,
                                      QR_SizeT&            i3,
                                const QR_Vertex::IBuffer*& pBuffer) const
{
    i1      = m_Index[0];
    i2      = m_Index[1];
    i3      = m_Index[2];
    pBuffer = m_pVertexBuffer;
}
//------------------------------------------------------------------------------
