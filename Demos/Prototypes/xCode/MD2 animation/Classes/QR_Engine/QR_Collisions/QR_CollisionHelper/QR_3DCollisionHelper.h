/******************************************************************************
 * ==> QR_3DCollisionHelper --------------------------------------------------*
 ******************************************************************************
 * Description : Helper for collision detection in 3D world                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_3DCollisionHelperH
#define QR_3DCollisionHelperH

// qr engine
#include "QR_Types.h"
#include "QR_Vertex.h"
#include "QR_Polygon.h"
#include "QR_Box.h"
#include "QR_Sphere.h"
#include "QR_Ray.h"

/**
* 3D Collisions detections toolbox
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_3DCollisionHelper
{
    public:
        /**
        * Polygon type enumeration
        */
        enum IEPolygonType
        {
            IE_PT_Simple,
            IE_PT_Indexed,
        };

        /**
        * Constructor
        */
        QR_3DCollisionHelper();

        /**
        * Destructor
        */
        virtual ~QR_3DCollisionHelper();

        /**
        * Tests and gets collision between a sphere and a box
        *@param sphere - sphere to test
        *@param box - box to test
        *@return true if objects are in collision, otherwise false
        */
        static bool GetSphereBoxCollision(const QR_Sphere& sphere,
                                          const QR_Box&    box);

        /**
        * Tests and gets collision between a sphere and a plane
        *@param sphere - sphere to test
        *@param plane - plane to test
        *@return true if objects are in collision, otherwise false
        */
        static bool GetSpherePlaneCollision(const QR_Sphere& sphere,
                                            const QR_PlaneP& plane);

        /**
        * Tests and gets collision between a box and a plane
        *@param box - box to test
        *@param plane - plane to test
        *@return true if objects are in collision, otherwise false
        */
        static bool GetBoxPlaneCollision(const QR_Box&    box,
                                         const QR_PlaneP& plane);

        /**
        * Tests collision between a ray and a box
        *@param ray - ray to check
        *@param box - box to check
        *@return true if objects are in collision, otherwise false
        */
        static bool GetRayBoxCollision(const QR_RayP& ray, const QR_Box& box);

        /**
        * Tests collision between a ray and a polygon
        *@param ray - ray to check
        *@param polygon - polygon to check
        *@return true if objects are in collision, otherwise false
        */
        static bool GetTriRayCollision(const QR_RayP& ray, const QR_Polygon& polygon);

        /**
        * Tests collision between a sphere and a polygon
        *@param sphere - sphere representing player
        *@param polygon - polygon to check
        *@param[out] slidingPlane - sliding plane
        *@return true if objects are in collision, otherwise false
        */
        static bool GetTriSphereCollision(const QR_Sphere&  sphere,
                                          const QR_Polygon& polygon,
                                                QR_PlaneP&  slidingPlane);

        /**
        * Gets sliding point (correction of the player position using a plane)
        *@param sphere - sphere representing player
        *@param slidingPlane - sliding plane
        *@return sliding point
        */
        static QR_Vector3DP GetSlidingPoint(const QR_Sphere& sphere,
                                            const QR_PlaneP& slidingPlane);

        /**
        * Calculates and gets the projection of a given point on a given segment
        *@param segStart - segment start
        *@param segEnd - segment end
        *@param point - point for which projection must be calculated
        *@return calculated point
        */
        static QR_Vector3DP ClosestPointOnLine(const QR_Vector3DP& segStart,
                                               const QR_Vector3DP& segEnd,
                                               const QR_Vector3DP& point);

        /**
        * Calculates and gets the projection of a given point on a given polygon
        *@param point - point for which projection must be calculated
        *@param polygon - polygon
        *@return the calculated point
        *@throw Exception on error
        */
        static QR_Vector3DP ClosestPointOnTriangle(const QR_Vector3DP& point,
                                                   const QR_Polygon&   polygon);

        /**
        * Calculates bounding box from polygon list
        *@param polygonList - polygon list
        *@return bounding box
        */
        static QR_Box CalculateBoundingBox(const QR_PolygonList& polygonList);

        /**
        * Calculates bounding sphere from polygon list
        *@param polygonList - polygon list
        *@return bounding sphere
        */
        static QR_Sphere CalculateBoundingSphere(const QR_PolygonList& polygonList);

        /**
        * Gets polygons from vertex
        *@param pVertex - source vertex descriptor, contains buffer to get from
        *@param polygonType - polygon type to generate
        *@param[out] polygonList - polygon list to populate
        *@return true on success, otherwise false
        *@note Polygon list content should be deleted when useless
        */
        static bool GetPolygons(const QR_Vertex*      pVertex,
                                      IEPolygonType   polygonType,
                                      QR_PolygonList& polygonList);

    private:
        /**
        * Gets polygon from vertex
        *@param pVertex - source vertex descriptor, contains buffer to get from
        *@param v1 - first vertex index
        *@param v2 - second vertex index
        *@param v3 - third vertex index
        *@param polygonType - polygon type to generate
        *@return polygon
        */
        static QR_Polygon* GetPolygon(const QR_Vertex*    pVertex,
                                            QR_SizeT      v1,
                                            QR_SizeT      v2,
                                            QR_SizeT      v3,
                                            IEPolygonType polygonType);

        /**
        * Gets simple polygon from vertex buffer
        *@param pVertex - vertex info containing the vertex buffer to get from
        *@param v1 - first vertex index
        *@param v2 - second vertex index
        *@param v3 - third vertex index
        *@return polygon
        */
        static QR_Polygon* GetSimplePolygon(const QR_Vertex* pVertex,
                                                  QR_SizeT   v1,
                                                  QR_SizeT   v2,
                                                  QR_SizeT   v3);

        /**
        * Gets indexed polygon from vertex buffer
        *@param pVertex - vertex info containing the vertex buffer to get from
        *@param v1 - first vertex index
        *@param v2 - second vertex index
        *@param v3 - third vertex index
        *@return polygon
        */
        static QR_Polygon* GetIndexedPolygon(const QR_Vertex* pVertex,
                                                   QR_SizeT   v1,
                                                   QR_SizeT   v2,
                                                   QR_SizeT   v3);
};

#endif // QR_3DCollisionHelperH
