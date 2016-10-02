/******************************************************************************
 * ==> QR_Polygon ------------------------------------------------------------*
 ******************************************************************************
 * Description : Basic polygon class                                          *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_PolygonH
#define QR_PolygonH

// std
#include <list>

// qr engine
#include "QR_Types.h"
#include "QR_CollideShape3D.h"
#include "QR_Matrix16.h"
#include "QR_Plane.h"

/**
* Class representing a polygon
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Polygon : public QR_CollideShape3D<M_Precision>
{
    public:
        /**
        * Constructor
        */
        QR_Polygon();

        /**
        * Destructor
        */
        virtual ~QR_Polygon();

        /**
        * Gets vertex at index
        *@param index - vertex index
        *@return corresponding vertex, empty vertex if not found
        */
        virtual QR_Vector3DP GetVertex(QR_UInt8 index) const = 0;

        /**
        * Sets vertex
        *@param index - vertex index to set
        *@param vertex - vertex value
        */
        virtual void SetVertex(QR_UInt8 index, const QR_Vector3DP& vertex) = 0;

        /**
        * Gets first polygon vertex
        *@return first polygon vertex
        */
        virtual QR_Vector3DP GetVertex1() const = 0;

        /**
        * Sets first polygon vertex
        *@param value - first polygon vertex value
        */
        virtual void SetVertex1(const QR_Vector3DP& value) = 0;

        /**
        * Gets second polygon vertex
        *@return second polygon vertex
        */
        virtual QR_Vector3DP GetVertex2() const = 0;

        /**
        * Sets second polygon vertex
        *@param value - second polygon vertex value
        */
        virtual void SetVertex2(const QR_Vector3DP& value) = 0;

        /**
        * Gets third polygon vertex
        *@return third polygon vertex
        */
        virtual QR_Vector3DP GetVertex3() const = 0;

        /**
        * Sets third polygon vertex
        *@param value - third polygon vertex value
        */
        virtual void SetVertex3(const QR_Vector3DP& value) = 0;

        /**
        * Creates and returns a clone of the polygon
        *@return a clone of the polygon
        *@note The returned polygon should be deleted when useless
        */
        virtual QR_Polygon* GetClone() const = 0;

        /**
        * Gets the polygon plane
        *@return the polygon plane
        */
        virtual QR_PlaneP GetPlane() const;

        /**
        * Calculates and returns the center point of the polygon
        *@return the center point of the polygon
        */
        virtual QR_Vector3DP GetCenter() const;

        /**
        * Checks if a point is inside polygon
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@param z - point z coordinate
        *@return true if point is inside polygon, otherwise false
        */
        virtual bool Inside(const M_Precision& x,
                            const M_Precision& y,
                            const M_Precision& z) const;

        /**
        * Checks if a point is inside polygon
        *@param point - point coordinate
        *@return true if point is inside polygon, otherwise false
        */
        virtual bool Inside(const QR_Vector3DP& point) const;

        /**
        * Checks if polygon intersects with another polygon
        *@param pOther - other polygon to check
        *@return true if polygons intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape3D<M_Precision>* pOther) const;

        /**
        * Calculates distance to polygon
        *@param point - point from which the distance must be calculated
        *@return distance to polygon
        */
        virtual M_Precision DistanceTo(const QR_Vector3DP& point) const;

    private:
        /**
        * Checks if a vector is between start and end limits
        *@param value - value to check
        *@param start - start limit
        *@param end - end limit
        *@param tolerance - tolerance
        *@return true if value is between limits, otherwise false
        */
        bool IsBetween(const QR_Vector3DP& value,
                       const QR_Vector3DP& start,
                       const QR_Vector3DP& end,
                       const M_Precision&  tolerance) const;

        /**
        * Checks if a value is between start and end limits
        *@param value - value to check
        *@param start - start limit
        *@param end - end limit
        *@param tolerance - tolerance
        *@return true if value is between limits, otherwise false
        */
        bool IsBetween(const M_Precision& value,
                       const M_Precision& start,
                       const M_Precision& end,
                       const M_Precision& tolerance) const;
};

/**
* Polygon list
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
typedef std::list<QR_Polygon*> QR_PolygonList;

#endif // QR_PolygonH
