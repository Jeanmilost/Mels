/******************************************************************************
 * ==> QR_SimplePolygon ------------------------------------------------------*
 ******************************************************************************
 * Description : Polygon that contains vertex coordinates as real values      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_SimplePolygon.h"

//------------------------------------------------------------------------------
// QR_SimplePolygon - c++ cross-platform
//------------------------------------------------------------------------------
QR_SimplePolygon::QR_SimplePolygon() : QR_Polygon()
{}
//------------------------------------------------------------------------------
QR_SimplePolygon::QR_SimplePolygon(const QR_Vector3DP& vertex1,
                                   const QR_Vector3DP& vertex2,
                                   const QR_Vector3DP& vertex3) :
    QR_Polygon()
{
    m_Vertex[0] = vertex1;
    m_Vertex[1] = vertex2;
    m_Vertex[2] = vertex3;
}
//------------------------------------------------------------------------------
QR_SimplePolygon::~QR_SimplePolygon()
{}
//------------------------------------------------------------------------------
QR_Vector3DP QR_SimplePolygon::GetVertex(QR_UInt8 index) const
{
    // search for index to get
    switch (index)
    {
        case 0:
        case 1:
        case 2:  return m_Vertex[index];
        default: return QR_Vector3DP();
    }
}
//------------------------------------------------------------------------------
void QR_SimplePolygon::SetVertex(QR_UInt8 index, const QR_Vector3DP& vertex)
{
    // search for index to set
    switch (index)
    {
        case 0:
        case 1:
        case 2: m_Vertex[index] = vertex; return;
    }
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_SimplePolygon::GetVertex1() const
{
    return m_Vertex[0];
}
//------------------------------------------------------------------------------
void QR_SimplePolygon::SetVertex1(const QR_Vector3DP& value)
{
    m_Vertex[0] = value;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_SimplePolygon::GetVertex2() const
{
    return m_Vertex[1];
}
//------------------------------------------------------------------------------
void QR_SimplePolygon::SetVertex2(const QR_Vector3DP& value)
{
    m_Vertex[1] = value;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_SimplePolygon::GetVertex3() const
{
    return m_Vertex[2];
}
//------------------------------------------------------------------------------
void QR_SimplePolygon::SetVertex3(const QR_Vector3DP& value)
{
    m_Vertex[2] = value;
}
//------------------------------------------------------------------------------
QR_Polygon* QR_SimplePolygon::GetClone() const
{
    // copies the polygon, then returns the copy
    return new QR_SimplePolygon(m_Vertex[0], m_Vertex[1], m_Vertex[2]);
}
//------------------------------------------------------------------------------
QR_Polygon* QR_SimplePolygon::ApplyMatrix(const QR_Matrix16P& matrix) const
{
    // build a new polygon transforming all vertices of the polygon using
    // given matrix, and return new built polygon
    return new QR_SimplePolygon(matrix.Transform(m_Vertex[0]),
                                matrix.Transform(m_Vertex[1]),
                                matrix.Transform(m_Vertex[2]));
}
//------------------------------------------------------------------------------
