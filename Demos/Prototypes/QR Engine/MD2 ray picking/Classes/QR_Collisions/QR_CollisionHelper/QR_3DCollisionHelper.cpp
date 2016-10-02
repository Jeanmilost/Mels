/******************************************************************************
 * ==> QR_3DCollisionHelper --------------------------------------------------*
 ******************************************************************************
 * Description : Helper for collision detection in 3D world                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_3DCollisionHelper.h"

// std
#include <cmath>
#include <limits>

// qr engine
#include "QR_Exception.h"
#include "QR_SimplePolygon.h"
#include "QR_IndexedPolygon.h"

//------------------------------------------------------------------------------
// Class QR_3DCollisionHelper - c++ cross-platform
//------------------------------------------------------------------------------
QR_3DCollisionHelper::QR_3DCollisionHelper()
{}
//------------------------------------------------------------------------------
QR_3DCollisionHelper::~QR_3DCollisionHelper()
{}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetSphereBoxCollision(const QR_Sphere& sphere,
                                                 const QR_Box&    box)
{
    QR_Float d = 0;

    // find the square of the distance from the sphere to the box on the x axis
    if (sphere.m_Center.m_X < box.m_Min.m_X)
        d += std::pow(sphere.m_Center.m_X - box.m_Min.m_X, 2);
    else
    if (sphere.m_Center.m_X > box.m_Max.m_X)
        d += std::pow(sphere.m_Center.m_X - box.m_Max.m_X, 2);

    // find the square of the distance from the sphere to the box on the y axis
    if (sphere.m_Center.m_Y < box.m_Min.m_Y)
        d += std::pow(sphere.m_Center.m_Y - box.m_Min.m_Y, 2);
    else
    if (sphere.m_Center.m_Y > box.m_Max.m_Y)
        d += std::pow(sphere.m_Center.m_Y - box.m_Max.m_Y, 2);

    // find the square of the distance from the sphere to the box on the z axis
    if (sphere.m_Center.m_Z < box.m_Min.m_Z)
        d += std::pow(sphere.m_Center.m_Z - box.m_Min.m_Z, 2);
    else
    if (sphere.m_Center.m_Z > box.m_Max.m_Z)
        d += std::pow(sphere.m_Center.m_Z - box.m_Max.m_Z, 2);

    return (d <= std::pow(sphere.m_Radius, 2));
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetSpherePlaneCollision(const QR_Sphere& sphere,
                                                   const QR_PlaneP& plane)
{
    // calculate distance between sphere center and plane
    M_Precision sphereToPlane = plane.DistanceTo(sphere.m_Center);

    QR_Vector3DP sphereNormal;

    // is sphere before or after plane?
    if (sphereToPlane < 0.0f)
        // get sphere normal
        sphereNormal = QR_Vector3DP(plane.m_A, plane.m_B, plane.m_C);
    else
        // get sphere normal, invert values to conserve correct direction
        sphereNormal = QR_Vector3DP(-plane.m_A, -plane.m_B, -plane.m_C);

    // calculate where vector starting from sphere center toward the plane
    // crosses the sphere border
    QR_Vector3DP pointOnSphere(sphere.m_Center.m_X + (sphere.m_Radius * sphereNormal.m_X),
                               sphere.m_Center.m_Y + (sphere.m_Radius * sphereNormal.m_Y),
                               sphere.m_Center.m_Z + (sphere.m_Radius * sphereNormal.m_Z));

    // calculate where vector starting from sphere center toward the plane
    // crosses the plane
    M_Precision radiusToPlane = plane.DistanceTo(pointOnSphere);

    // sphere cross plane if both points are located on each side of the plane
    // or if one point on the plane
    return ((sphereToPlane <  0.0f && radiusToPlane >  0.0f) ||
            (sphereToPlane >  0.0f && radiusToPlane <  0.0f) ||
             sphereToPlane == 0.0f || radiusToPlane == 0.0f);
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetBoxPlaneCollision(const QR_Box&    box,
                                                const QR_PlaneP& plane)
{
    // get the 8 box vertexes
    QR_Vector3DP vertexes[8];
    vertexes[0] = box.m_Min;
    vertexes[1] = QR_Vector3DP(box.m_Min.m_X, box.m_Max.m_Y, box.m_Min.m_Z);
    vertexes[2] = QR_Vector3DP(box.m_Max.m_X, box.m_Min.m_Y, box.m_Min.m_Z);
    vertexes[3] = QR_Vector3DP(box.m_Max.m_X, box.m_Max.m_Y, box.m_Min.m_Z);
    vertexes[4] = QR_Vector3DP(box.m_Min.m_X, box.m_Min.m_Y, box.m_Max.m_Z);
    vertexes[5] = QR_Vector3DP(box.m_Min.m_X, box.m_Max.m_Y, box.m_Max.m_Z);
    vertexes[6] = QR_Vector3DP(box.m_Max.m_X, box.m_Min.m_Y, box.m_Max.m_Z);
    vertexes[7] = box.m_Max;

    M_Precision refLength = 0.0f;

    // iterate through box vertexes
    for (QR_UInt8 i = 0; i < 8; ++i)
    {
        // calculate distance between current box vertex and plane
        M_Precision length = plane.DistanceTo(vertexes[i]);

        // is vertex on plane?
        if (length == 0.0f)
            return true;

        // is reference initialized?
        if (refLength == 0.0f)
        {
            refLength = length;
            continue;
        }

        // box cross plane if at least one vertex is on the other plane side
        if ((length < 0.0f && refLength > 0.0f) ||
            (length > 0.0f && refLength < 0.0f))
            return true;
    }

    return false;
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetRayBoxCollision(const QR_RayP& ray, const QR_Box& box)
{
    // get infinite value
    const double inf = std::numeric_limits<double>::infinity();

    // calculate points where ray intersects box
    const double tx1 = (ray.m_InvDir.m_X != inf) ? ((box.m_Min.m_X - ray.m_Pos.m_X) * ray.m_InvDir.m_X) :
            ((box.m_Min.m_X - ray.m_Pos.m_X) < 0.0f ? -inf : inf);
    const double tx2 = (ray.m_InvDir.m_X != inf) ? ((box.m_Max.m_X - ray.m_Pos.m_X) * ray.m_InvDir.m_X) :
            ((box.m_Max.m_X - ray.m_Pos.m_X) < 0.0f ? -inf : inf);
    const double ty1 = (ray.m_InvDir.m_Y != inf) ? ((box.m_Min.m_Y - ray.m_Pos.m_Y) * ray.m_InvDir.m_Y) :
            ((box.m_Min.m_Y - ray.m_Pos.m_Y) < 0.0f ? -inf : inf);
    const double ty2 = (ray.m_InvDir.m_Y != inf) ? ((box.m_Max.m_Y - ray.m_Pos.m_Y) * ray.m_InvDir.m_Y) :
            ((box.m_Max.m_Y - ray.m_Pos.m_Y) < 0.0f ? -inf : inf);
    const double tz1 = (ray.m_InvDir.m_Z != inf) ? ((box.m_Min.m_Z - ray.m_Pos.m_Z) * ray.m_InvDir.m_Z) :
            ((box.m_Min.m_Z - ray.m_Pos.m_Z) < 0.0f ? -inf : inf);
    const double tz2 = (ray.m_InvDir.m_Z != inf) ? ((box.m_Max.m_Z - ray.m_Pos.m_Z) * ray.m_InvDir.m_Z) :
            ((box.m_Max.m_Z - ray.m_Pos.m_Z) < 0.0f ? -inf : inf);

    // calculate near/far intersection on each axis
    const double txn = std::min(tx1, tx2);
    const double txf = std::max(tx1, tx2);
    const double tyn = std::min(ty1, ty2);
    const double tyf = std::max(ty1, ty2);
    const double tzn = std::min(tz1, tz2);
    const double tzf = std::max(tz1, tz2);

    // calculate final near/far intersection point
    const double tnear = std::max(txn, std::max(tyn, tzn));
    const double tfar  = std::min(txf, std::min(tyf, tzf));

    // check if ray intersects box
    return (tfar >= tnear);
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetTriRayCollision(const QR_RayP& ray, const QR_Polygon& polygon)
{
    // create a plane using the 3 vertices of the polygon
    QR_PlaneP polygonPlane = polygon.GetPlane();

    QR_Vector3DP pointOnPlane;

    // calculate the intersection point
    if (!polygonPlane.IntersectRay(ray.m_Pos, ray.m_Dir, pointOnPlane))
        return false;

    // check if calculated point is inside the polygon
    return polygon.Inside(pointOnPlane);
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetTriSphereCollision(const QR_Sphere&  sphere,
                                                 const QR_Polygon& polygon,
                                                       QR_PlaneP&  slidingPlane)
{
    // create a plane using the 3 vertices of the polygon
    QR_PlaneP polygonPlane = polygon.GetPlane();

    // calculate the distance between the center of the sphere and the plane
    M_Precision testPoint1 = polygonPlane.DistanceTo(sphere.m_Center);

    QR_Vector3DP sphereNormal;

    // calculate the normal of the distance sphere-plane using the positive or
    // negative value of the calculated distance between plane and position
    if (testPoint1 < 0.0f)
        sphereNormal = QR_Vector3DP(polygonPlane.m_A,
                                    polygonPlane.m_B,
                                    polygonPlane.m_C);
    else
        sphereNormal = QR_Vector3DP(-polygonPlane.m_A,
                                    -polygonPlane.m_B,
                                    -polygonPlane.m_C);

    // calculate the point who the segment from center of sphere in the
    // direction of the plane will cross the border of the sphere
    QR_Vector3DP pointOnSphere =
            QR_Vector3DP(sphere.m_Center.m_X + (sphere.m_Radius * sphereNormal.m_X),
                         sphere.m_Center.m_Y + (sphere.m_Radius * sphereNormal.m_Y),
                         sphere.m_Center.m_Z + (sphere.m_Radius * sphereNormal.m_Z));

    // calculate the distance between the border of the sphere and the plane
    M_Precision testPoint2 = polygonPlane.DistanceTo(pointOnSphere);

    // if the test points are on each side of the plane, then the sphere cross
    // the plane. We assume that the segment from the center of the sphere to
    // the direction of the plane can never be co-planar
    if ((testPoint1 <= 0.0f && testPoint2 >= 0.0f) ||
        (testPoint2 <= 0.0f && testPoint1 >= 0.0f))
    {
        QR_Vector3DP pointOnPlane;

        // calculate who the segment cross the plane
        if (testPoint1 == 0.0f)
            // if testPoint1 is equal to 0, the center of the sphere cross the
            // plane
            pointOnPlane = sphere.m_Center;
        else
        if (testPoint2 == 0.0f)
            // if testPoint2 is equal to 0, the border of the sphere cross the
            // plane
            pointOnPlane = pointOnSphere;
        else
            // calculate the intersection point
            polygonPlane.IntersectLine(sphere.m_Center,
                                       pointOnSphere,
                                       pointOnPlane);

        // check if calculated point is inside the polygon
        if (polygon.Inside(pointOnPlane))
        {
            // if yes, the sphere collide the polygon. In this case, we copy
            // the plane and we returns true
            slidingPlane = polygonPlane;
            return true;
        }
        else
        {
            // otherwise check if the sphere collide the border of the polygon.
            // First we calculate the test point on the border of the polygon
            QR_Vector3DP pointOnTriangle = ClosestPointOnTriangle(pointOnPlane,
                                                                  polygon);

            // check if calculated point is inside the sphere
            if (sphere.Inside(pointOnTriangle))
            {
                // if yes, the sphere collide the polygon. In this case, we
                // copy the plane and we returns true
                slidingPlane = polygonPlane;
                return true;
            }
        }
    }

    // no collision was found
    return false;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_3DCollisionHelper::GetSlidingPoint(const QR_Sphere& sphere,
                                                   const QR_PlaneP& slidingPlane)
{
    QR_PlaneP plane = slidingPlane;

    // calculate the distance between the center of the sphere and the plane
    M_Precision distanceToPlane = plane.DistanceTo(sphere.m_Center);

    // check if value is negative
    if (distanceToPlane < 0.0f)
    {
        // invert the plane
        plane.m_A = -plane.m_A;
        plane.m_B = -plane.m_B;
        plane.m_C = -plane.m_C;
        plane.m_D = -plane.m_D;
    }

    // calculate the direction of the segment position - plane
    QR_Vector3DP planeRatio(sphere.m_Radius * plane.m_A,
                            sphere.m_Radius * plane.m_B,
                            sphere.m_Radius * plane.m_C);

    // calculate who the segment perpendicular to the plane, from the center
    // of the sphere, cross the collision sphere. Normally this point is beyond
    // the plane
    QR_Vector3DP pointBeyondPlane(sphere.m_Center.m_X - planeRatio.m_X,
                                  sphere.m_Center.m_Y - planeRatio.m_Y,
                                  sphere.m_Center.m_Z - planeRatio.m_Z);

    QR_Vector3DP pointOnPlane;

    // calculate the point who the segment "center of the sphere - point beyond
    // the plane" cross the collision plane
    slidingPlane.IntersectLine(sphere.m_Center, pointBeyondPlane, pointOnPlane);

    // from point calculated below, we add the radius of the sphere, and we
    // returns the value
    return QR_Vector3DP(pointOnPlane.m_X + planeRatio.m_X,
                        pointOnPlane.m_Y + planeRatio.m_Y,
                        pointOnPlane.m_Z + planeRatio.m_Z);
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_3DCollisionHelper::ClosestPointOnLine(const QR_Vector3DP& segStart,
                                                      const QR_Vector3DP& segEnd,
                                                      const QR_Vector3DP& point)
{
    // calculate the distance between the test point and the segment
    QR_Vector3DP PToStart  = point  - segStart;
    QR_Vector3DP length    = segEnd - segStart;
    M_Precision  segLength = length.Length();

    // calculate the direction of the segment
    QR_Vector3DP normalizedLength = length.Normalize();

    // calculate the projection of the point on the segment
    M_Precision angle = normalizedLength.Dot(PToStart);

    // check if projection is before the segment
    if (angle < 0.0f)
        return segStart;
    else
    // check if projection is after the segment
    if (angle > segLength)
        return segEnd;
    else
    {
        // calculate the position of the projection on the segment
        QR_Vector3DP p(normalizedLength.m_X * angle,
                       normalizedLength.m_Y * angle,
                       normalizedLength.m_Z * angle);

        // calculate and returns the point coordinate on the segment
        return (segStart + p);
    }
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_3DCollisionHelper::ClosestPointOnTriangle(const QR_Vector3DP& point,
                                                          const QR_Polygon&   polygon)
{
    // get polygon vertices
    const QR_Vector3DP vertex1 = polygon.GetVertex1();
    const QR_Vector3DP vertex2 = polygon.GetVertex2();
    const QR_Vector3DP vertex3 = polygon.GetVertex3();

    // calculate the projections points on each edge of the triangle
    QR_Vector3DP rab = ClosestPointOnLine(vertex1, vertex2, point);
    QR_Vector3DP rbc = ClosestPointOnLine(vertex2, vertex3, point);
    QR_Vector3DP rca = ClosestPointOnLine(vertex3, vertex1, point);

    // calculate the distances between points below and test point
    QR_Vector3DP vAB = point - rab;
    QR_Vector3DP vBC = point - rbc;
    QR_Vector3DP vCA = point - rca;

    // calculate the length of each segments
    M_Precision dAB = vAB.Length();
    M_Precision dBC = vBC.Length();
    M_Precision dCA = vCA.Length();

    // calculate the shortest distance
    M_Precision min = dAB;

    QR_Vector3DP result = rab;

    // check if dBC is shortest
    if (dBC < min)
    {
        min    = dBC;
        result = rbc;
    }

    // check if dCA is shortest
    if (dCA < min)
        result = rca;

    return result;
}
//------------------------------------------------------------------------------
QR_Box QR_3DCollisionHelper::CalculateBoundingBox(const QR_PolygonList& polygonList)
{
    QR_Box box;

    bool initialized = false;

    // iterate through polygons
    for (QR_PolygonList::const_iterator it = polygonList.begin();
            it != polygonList.end(); ++it)
        for (QR_UInt8 i = 0; i < 3; ++i)
        {
            // get vertex
            const QR_Vector3DP vertex = (*it)->GetVertex(i);

            // are values initialized?
            if (!initialized)
            {
                // initialize bounding box with first vertex
                box.m_Min   = QR_Vector3DP(vertex.m_X, vertex.m_Y, vertex.m_Z);
                box.m_Max   = QR_Vector3DP(vertex.m_X, vertex.m_Y, vertex.m_Z);
                initialized = true;
                continue;
            }

            // search for box min edge
            box.m_Min.m_X = std::min(box.m_Min.m_X, vertex.m_X);
            box.m_Min.m_Y = std::min(box.m_Min.m_Y, vertex.m_Y);
            box.m_Min.m_Z = std::min(box.m_Min.m_Z, vertex.m_Z);

            // search for box max edge
            box.m_Max.m_X = std::max(box.m_Max.m_X, vertex.m_X);
            box.m_Max.m_Y = std::max(box.m_Max.m_Y, vertex.m_Y);
            box.m_Max.m_Z = std::max(box.m_Max.m_Z, vertex.m_Z);
        }

    return box;
}
//------------------------------------------------------------------------------
QR_Sphere QR_3DCollisionHelper::CalculateBoundingSphere(const QR_PolygonList& polygonList)
{
    QR_Vector3DP vMax;
    QR_Vector3DP vMin;

    bool initialized = false;

    // iterate through polygons
    for (QR_PolygonList::const_iterator it = polygonList.begin();
            it != polygonList.end(); ++it)
        for (QR_UInt8 i = 0; i < 3; ++i)
        {
            // get vertex
            const QR_Vector3DP vertex = (*it)->GetVertex(i);

            // are values initialized?
            if (!initialized)
            {
                // initialize bounding box with first vertex
                vMin        = QR_Vector3DP(vertex.m_X, vertex.m_Y, vertex.m_Z);
                vMax        = QR_Vector3DP(vertex.m_X, vertex.m_Y, vertex.m_Z);
                initialized = true;
                continue;
            }

            // search for min edge
            vMin.m_X = std::min(vMin.m_X, vertex.m_X);
            vMin.m_Y = std::min(vMin.m_Y, vertex.m_Y);
            vMin.m_Z = std::min(vMin.m_Z, vertex.m_Z);

            // search for max edge
            vMax.m_X = std::max(vMax.m_X, vertex.m_X);
            vMax.m_Y = std::max(vMax.m_Y, vertex.m_Y);
            vMax.m_Z = std::max(vMax.m_Z, vertex.m_Z);
        }

    QR_Sphere sphere;

    // calculate sphere center
    sphere.m_Center = QR_Vector3DP((vMin.m_X + vMax.m_X) / 2.0f,
                                   (vMin.m_Y + vMax.m_Y) / 2.0f,
                                   (vMin.m_Z + vMax.m_Z) / 2.0f);

    // calculate sphere radius
    sphere.m_Radius = QR_Vector3DP((vMax.m_X - vMin.m_X) / 2.0f,
                                   (vMax.m_Y - vMin.m_Y) / 2.0f,
                                   (vMax.m_Z - vMin.m_Z) / 2.0f).Length();

    return sphere;
}
//------------------------------------------------------------------------------
bool QR_3DCollisionHelper::GetPolygons(const QR_Vertex*      pVertex,
                                             IEPolygonType   polygonType,
                                             QR_PolygonList& polygonList)
{
    const QR_SizeT vertexCount = pVertex->m_Buffer.size();

    // no data to extract from?
    if (!vertexCount)
        return true;

    // search for vertex type
    switch (pVertex->m_Type)
    {
        case QR_Vertex::IE_VT_Triangles:
        {
            // calculate iteration step
            const QR_SizeT step = (pVertex->m_Stride * 3);

            // iterate through source vertexes
            for (QR_SizeT i = 0; i < vertexCount; i += step)
                // extract polygon from source buffer
                polygonList.push_back(GetPolygon(pVertex,
                                                 i,
                                                 i +  pVertex->m_Stride,
                                                 i + (pVertex->m_Stride * 2),
                                                 polygonType));

            return true;
        }

        case QR_Vertex::IE_VT_TriangleStrip:
        {
            // calculate length to read in triangle strip buffer
            const QR_SizeT stripLength = (vertexCount - (pVertex->m_Stride * 2));
                  QR_SizeT index       = 0;

            // iterate through source vertexes
            for (QR_SizeT i = 0; i < stripLength; i += pVertex->m_Stride)
            {
                // extract polygon from source buffer, revert odd polygons
                if (!index || !(index % 2))
                    polygonList.push_back(GetPolygon(pVertex,
                                                     i,
                                                     i +  pVertex->m_Stride,
                                                     i + (pVertex->m_Stride * 2),
                                                     polygonType));
                else
                    polygonList.push_back(GetPolygon(pVertex,
                                                     i +  pVertex->m_Stride,
                                                     i,
                                                     i + (pVertex->m_Stride * 2),
                                                     polygonType));

                ++index;
            }

            return true;
        }

        case QR_Vertex::IE_VT_TriangleFan:
        {
            // calculate length to read in triangle fan buffer
            const QR_SizeT fanLength = (vertexCount - pVertex->m_Stride);

            // iterate through source vertexes
            for (QR_SizeT i = pVertex->m_Stride; i < fanLength; i += pVertex->m_Stride)
                // extract polygon from source buffer
                polygonList.push_back(GetPolygon(pVertex,
                                                 0,
                                                 i,
                                                 i + pVertex->m_Stride,
                                                 polygonType));

            return true;
        }

        case QR_Vertex::IE_VT_Quads:
        {
            // calculate iteration step
            const QR_SizeT step = (pVertex->m_Stride * 4);

            // iterate through source vertexes
            for (QR_SizeT i = 0; i < vertexCount; i += step)
            {
                // calculate vertexes position
                const QR_SizeT v1 = i;
                const QR_SizeT v2 = i +  pVertex->m_Stride;
                const QR_SizeT v3 = i + (pVertex->m_Stride * 2);
                const QR_SizeT v4 = i + (pVertex->m_Stride * 3);

                // extract polygons from source buffer
                polygonList.push_back(GetPolygon(pVertex, v1, v2, v3, polygonType));
                polygonList.push_back(GetPolygon(pVertex, v3, v2, v4, polygonType));
            }

            return true;
        }

        case QR_Vertex::IE_VT_QuadStrip:
        {
            // calculate iteration step
            const QR_SizeT step = (pVertex->m_Stride * 2);

            // calculate length to read in triangle strip buffer
            const QR_SizeT stripLength = (vertexCount - (pVertex->m_Stride * 2));

            // iterate through source vertexes
            for (QR_SizeT i = 0; i < stripLength; i += step)
            {
                // calculate vertexes position
                const QR_SizeT v1 = i;
                const QR_SizeT v2 = i +  pVertex->m_Stride;
                const QR_SizeT v3 = i + (pVertex->m_Stride * 2);
                const QR_SizeT v4 = i + (pVertex->m_Stride * 3);

                // extract polygons from source buffer
                polygonList.push_back(GetPolygon(pVertex, v1, v2, v3, polygonType));
                polygonList.push_back(GetPolygon(pVertex, v3, v2, v4, polygonType));
            }

            return true;
        }

        case QR_Vertex::IE_VT_Unknown:
        default:
            return false;
    }
}
//------------------------------------------------------------------------------
QR_Polygon* QR_3DCollisionHelper::GetPolygon(const QR_Vertex*    pVertex,
                                                   QR_SizeT      v1,
                                                   QR_SizeT      v2,
                                                   QR_SizeT      v3,
                                                   IEPolygonType polygonType)
{
    // dispatch polygon type to use
    switch (polygonType)
    {
        case IE_PT_Simple:  return GetSimplePolygon(pVertex, v1, v2, v3);
        case IE_PT_Indexed: return GetIndexedPolygon(pVertex, v1, v2, v3);
        default:            return NULL;
    }
}
//------------------------------------------------------------------------------
QR_Polygon* QR_3DCollisionHelper::GetSimplePolygon(const QR_Vertex* pVertex,
                                                         QR_SizeT   v1,
                                                         QR_SizeT   v2,
                                                         QR_SizeT   v3)
{
    // create polygon from vertex buffer
    return new QR_SimplePolygon(QR_Vector3DP(pVertex->m_Buffer[v1],
                                             pVertex->m_Buffer[v1 + 1],
                                             pVertex->m_Buffer[v1 + 2]),
                                QR_Vector3DP(pVertex->m_Buffer[v2],
                                             pVertex->m_Buffer[v2 + 1],
                                             pVertex->m_Buffer[v2 + 2]),
                                QR_Vector3DP(pVertex->m_Buffer[v3],
                                             pVertex->m_Buffer[v3 + 1],
                                             pVertex->m_Buffer[v3 + 2]));
}
//------------------------------------------------------------------------------
QR_Polygon* QR_3DCollisionHelper::GetIndexedPolygon(const QR_Vertex* pVertex,
                                                          QR_SizeT   v1,
                                                          QR_SizeT   v2,
                                                          QR_SizeT   v3)
{
    // create polygon from vertex buffer
    return new QR_IndexedPolygon(v1, v2, v3, &pVertex->m_Buffer);
}
//------------------------------------------------------------------------------
