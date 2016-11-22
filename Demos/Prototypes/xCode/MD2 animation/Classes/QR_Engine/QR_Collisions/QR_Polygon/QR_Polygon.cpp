/******************************************************************************
 * ==> QR_Polygon ------------------------------------------------------------*
 ******************************************************************************
 * Description : Basic polygon class                                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_Polygon.h"

// std
#include <limits>
#include <math.h>
#include <vector>
#include <cstring>

#ifdef _WIN32
    #include <cmath>
#endif

// qr engine
#include "QR_Exception.h"
#include "QR_Line.h"

//------------------------------------------------------------------------------
// QR_Polygon - c++ cross-platform
//------------------------------------------------------------------------------
QR_Polygon::QR_Polygon() : QR_CollideShape3D<M_Precision>()
{}
//------------------------------------------------------------------------------
QR_Polygon::~QR_Polygon()
{}
//------------------------------------------------------------------------------
QR_PlaneP QR_Polygon::GetPlane() const
{
    // calculates the plane from the values of the 3 vertices of the polygon
    return QR_PlaneP::FromPoints(GetVertex1(), GetVertex2(), GetVertex3());
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_Polygon::GetCenter() const
{
    const QR_Vector3DP v1 = GetVertex1();
    const QR_Vector3DP v2 = GetVertex2();
    const QR_Vector3DP v3 = GetVertex3();

    // calculates then returns the value of the midpoint of the polygon
    return QR_Vector3DP(((v1.m_X + v2.m_X + v3.m_X) / 3.0f),
                        ((v1.m_Y + v2.m_Y + v3.m_Y) / 3.0f),
                        ((v1.m_Z + v2.m_Z + v3.m_Z) / 3.0f));
}
//------------------------------------------------------------------------------
bool QR_Polygon::Inside(const M_Precision& x,
                        const M_Precision& y,
                        const M_Precision& z) const
{
    return Inside(QR_Vector3DP(x, y, z));
}
//------------------------------------------------------------------------------
bool QR_Polygon::Inside(const QR_Vector3DP& point) const
{
    /*
    * check if the point p is inside the polygon in the following manner:
    *
    *                  V1                         V1
    *                  /\                         /\
    *                 /  \                       /  \
    *                / *p \                  *P /    \
    *               /      \                   /      \
    *            V2 -------- V3             V2 -------- V3
    *
    * calculate the vectors between the point p and each polygon vertex, then
    * calculate the angle formed by each of these vectors. If the sum of the
    * angles is equal to a complete circle, i.e. 2 * pi in radians, then the
    * point p is inside the polygon limits, otherwise the point is outside. It
    * is assumed that the point to check belongs to the polygon's plane
    */
    const QR_Vector3DP nPToV1 = (GetVertex1() - point).Normalize();
    const QR_Vector3DP nPToV2 = (GetVertex2() - point).Normalize();
    const QR_Vector3DP nPToV3 = (GetVertex3() - point).Normalize();

    // calculate the angles using the dot product of each vectors. Limit range
    // to values between -1.0f and 1.0f
    const M_Precision a1 = std::max(std::min(nPToV1.Dot(nPToV2), 1.0f), -1.0f);
    const M_Precision a2 = std::max(std::min(nPToV2.Dot(nPToV3), 1.0f), -1.0f);
    const M_Precision a3 = std::max(std::min(nPToV3.Dot(nPToV1), 1.0f), -1.0f);

    // calculate the sum of all angles
    #ifdef __APPLE__
        const M_Precision angleResult =
                (M_Precision)acosf(a1) + (M_Precision)acosf(a2)
                        + (M_Precision)acosf(a3);
    #else
        const M_Precision angleResult =
                (M_Precision)std::acos(a1) + (M_Precision)std::acos(a2)
                        + (M_Precision)std::acos(a3);
    #endif

    // if sum is equal to 6.28 radians then point p is inside polygon. NOTE can
    // be higher due to precision errors in calculations
    return (angleResult >= 6.28f);
}
//------------------------------------------------------------------------------
bool QR_Polygon::Intersect(const QR_CollideShape3D<M_Precision>* pOther) const
{
    const QR_Polygon* pPolygon = dynamic_cast<const QR_Polygon*>(pOther);

    if (!pPolygon)
        M_THROW_EXCEPTION("Could not convert other object to polygon");

    // get this polygon vertices
    const QR_Vector3DP v1 = GetVertex1();
    const QR_Vector3DP v2 = GetVertex2();
    const QR_Vector3DP v3 = GetVertex3();

    // get other polygon vertices
    const QR_Vector3DP ov1 = pPolygon->GetVertex1();
    const QR_Vector3DP ov2 = pPolygon->GetVertex2();
    const QR_Vector3DP ov3 = pPolygon->GetVertex3();

    // get planes from polygons
    QR_PlaneP plane1 = QR_PlaneP::FromPoints(v1,  v2,  v3);
    QR_PlaneP plane2 = QR_PlaneP::FromPoints(ov1, ov2, ov3);

    // are planes merged?
    if (plane1 == plane2 || plane1 == -plane2)
    {
        // is any vertex inside other polygon?
        if (Inside(ov1)          || Inside(ov2)          || Inside(ov3) ||
            pPolygon->Inside(v1) || pPolygon->Inside(v2) || pPolygon->Inside(v3))
            return true;

        // create polygon lines
        QR_LineP v1v2Line(v1, v2);
        QR_LineP v2v3Line(v2, v3);
        QR_LineP v3v1Line(v3, v1);
        QR_LineP ov1ov2Line(ov1, ov2);
        QR_LineP ov2ov3Line(ov2, ov3);
        QR_LineP ov3ov1Line(ov3, ov1);
        QR_LineP output;

        // is shortest distance between lines equal to 0?
        if (v1v2Line.GetShortestDistance(ov1ov2Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v2v3Line.GetShortestDistance(ov1ov2Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v3v1Line.GetShortestDistance(ov1ov2Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v1v2Line.GetShortestDistance(ov2ov3Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v2v3Line.GetShortestDistance(ov2ov3Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v3v1Line.GetShortestDistance(ov2ov3Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v1v2Line.GetShortestDistance(ov3ov1Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v2v3Line.GetShortestDistance(ov3ov1Line) < M_Epsilon)
            return true;

        // is shortest distance between lines equal to 0?
        if (v3v1Line.GetShortestDistance(ov3ov1Line) < M_Epsilon)
            return true;

        return false;
    }

    // get plane normals
    QR_Vector3DP normal1(plane1.m_A, plane1.m_B, plane1.m_C);
    QR_Vector3DP normal2(plane2.m_A, plane2.m_B, plane2.m_C);

    // calculate angle between planes
    QR_Float planesDot = normal1.Dot(normal2);

    // are plane parallels?
    if (planesDot == 1.0f || planesDot == -1.0f)
        // planes are parallels but not merged, no collision is possible
        return false;

    // calculate distance from each first polygon vertex to second polygon plane
    QR_Float dist1v1 = plane2.DistanceTo(v1);
    QR_Float dist1v2 = plane2.DistanceTo(v2);
    QR_Float dist1v3 = plane2.DistanceTo(v3);

    // prepare list containing first polygon intersection points
    std::vector<QR_Vector3DP> p1pts;
    p1pts.reserve(2);

    // is first polygon V1 to V2 line segment intersects second polygon plane?
    if ((dist1v1 >= 0.0f && dist1v2 < 0.0f) || (dist1v1 < 0.0f && dist1v2 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane2.IntersectLine(v1, v2, p))
            p1pts.push_back(p);
    }

    // is first polygon V2 to V3 line segment intersects second polygon plane?
    if ((dist1v2 >= 0.0f && dist1v3 < 0.0f) || (dist1v2 < 0.0f && dist1v3 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane2.IntersectLine(v2, v3, p))
            p1pts.push_back(p);
    }

    // is first polygon V3 to V1 line segment intersects second polygon plane?
    if ((dist1v3 >= 0.0f && dist1v1 < 0.0f) || (dist1v3 < 0.0f && dist1v1 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane2.IntersectLine(v3, v1, p))
            p1pts.push_back(p);
    }

    // were the first polygon 2 intersection point found?
    if (p1pts.size() != 2)
        return false;

    // calculate distance from each second polygon vertex to first polygon plane
    M_Precision dist2v1 = plane1.DistanceTo(ov1);
    M_Precision dist2v2 = plane1.DistanceTo(ov2);
    M_Precision dist2v3 = plane1.DistanceTo(ov3);

    // prepare list containing second polygon intersection points
    std::vector<QR_Vector3DP> p2pts;
    p2pts.reserve(2);

    // is second polygon V1 to V2 line segment intersects first polygon plane?
    if ((dist2v1 >= 0.0f && dist2v2 < 0.0f) || (dist2v1 < 0.0f && dist2v2 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane1.IntersectLine(ov1, ov2, p))
            p2pts.push_back(p);
    }

    // is second polygon V2 to V3 line segment intersects first polygon plane?
    if ((dist2v2 >= 0.0f && dist2v3 < 0.0f) || (dist2v2 < 0.0f && dist2v3 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane1.IntersectLine(ov2, ov3, p))
            p2pts.push_back(p);
    }

    // is second polygon V3 to V1 line segment intersects first polygon plane?
    if ((dist2v3 >= 0.0f && dist2v1 < 0.0f) || (dist2v3 < 0.0f && dist2v1 >= 0.0f))
    {
        QR_Vector3DP p;

        // calculate intersection point and add to list on success
        if (plane1.IntersectLine(ov3, ov1, p))
            p2pts.push_back(p);
    }

    // were the second polygon 2 intersection point found?
    if (p2pts.size() != 2)
        return false;

    // first and second polygon intersection points are on the same line, so
    // check if calculated first and second polygon segments intersect
    return (IsBetween(p1pts[0], p2pts[0], p2pts[1], M_Epsilon) ||
            IsBetween(p1pts[1], p2pts[0], p2pts[1], M_Epsilon) ||
            IsBetween(p2pts[0], p1pts[0], p1pts[1], M_Epsilon) ||
            IsBetween(p2pts[1], p1pts[0], p1pts[1], M_Epsilon));
}
//------------------------------------------------------------------------------
M_Precision QR_Polygon::DistanceTo(const QR_Vector3DP& point) const
{
    M_Precision result = std::numeric_limits<double>::infinity();

    // check each line formed by polygon
    for (QR_UInt8 i = 0; i < 3; ++i)
    {
        // get previous line index to check with
        const QR_UInt8 previousIndex = i ? i - 1 : 2;

        // get line to check
        QR_LineP line(GetVertex(i), GetVertex(previousIndex));

        // calculate distance between point and polygon line
        const M_Precision dist = line.DistanceTo(point);

        // keep shortest distance
        if (dist < result)
            result = dist;
    }

    return result;
}
//------------------------------------------------------------------------------
bool QR_Polygon::IsBetween(const QR_Vector3DP& value,
                           const QR_Vector3DP& start,
                           const QR_Vector3DP& end,
                           const M_Precision&  tolerance) const
{
    // check if each vector component is between start and end limits
    return (IsBetween(value.m_X, start.m_X, end.m_X, tolerance) &&
            IsBetween(value.m_Y, start.m_Y, end.m_Y, tolerance) &&
            IsBetween(value.m_Z, start.m_Z, end.m_Z, tolerance));
}
//------------------------------------------------------------------------------
bool QR_Polygon::IsBetween(const M_Precision& value,
                           const M_Precision& start,
                           const M_Precision& end,
                           const M_Precision& tolerance) const
{
    // check if each value is between start and end limits considering tolerance
    return (value >= std::min(start, end) - tolerance &&
            value <= std::max(start, end) + tolerance);
}
//------------------------------------------------------------------------------
