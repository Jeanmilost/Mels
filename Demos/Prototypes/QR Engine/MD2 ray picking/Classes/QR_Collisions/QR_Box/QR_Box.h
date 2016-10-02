/******************************************************************************
 * ==> QR_Box ----------------------------------------------------------------*
 ******************************************************************************
 * Description : 3D box                                                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_BoxH
#define QR_BoxH

// qr engine
#include "QR_CollideShape3D.h"
#include "QR_Vector3D.h"
#include "QR_Plane.h"

/**
* Box
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Box : public QR_CollideShape3D<M_Precision>
{
    public:
        /**
        * Longest axis enumeration
        */
        enum IE_Axis
        {
            IE_X,
            IE_Y,
            IE_Z,
        };

        QR_Vector3DP m_Min;
        QR_Vector3DP m_Max;

        /**
        * Constructor
        */
        QR_Box();

        /**
        * Constructor
        *@param min - min corner
        *@param max - max corner
        */
        QR_Box(const QR_Vector3DP& min, const QR_Vector3DP& max);

        /**
        * Destructor
        */
        virtual ~QR_Box();

        /**
        * Gets center
        *@return center
        */
        virtual QR_Vector3DP GetCenter() const;

        /**
        * Gets longest axis
        *@return longest axis, @see IE_Axis enumeration
        */
        virtual IE_Axis GetLongestAxis() const;

        /**
        * Gets splitting plane
        *@param axis - axis for which splitting plane should be get
        *@return splitting plane
        */
        virtual QR_PlaneP GetSplittingPlane(IE_Axis axis) const;

        /**
        * Gets splitting plane
        *@param axis - axis for which splitting plane should be get
        *@param center - box center
        *@return splitting plane
        */
        virtual QR_PlaneP GetSplittingPlane(IE_Axis axis,
                                            const QR_Vector3DP& center) const;

        /**
        * Check if a point is inside box
        *@param x - point x coordinate
        *@param y - point y coordinate
        *@param z - point z coordinate
        *@return true if point is inside box, otherwise false
        */
        virtual bool Inside(const M_Precision& x, const M_Precision& y,
                const M_Precision& z) const;

        /**
        * Check if a point is inside box
        *@param point - point coordinate
        *@return true if point is inside box, otherwise false
        */
        virtual bool Inside(const QR_Vector3DP& point) const;

        /**
        * Check if box intersects with another box
        *@param pOther - other box to check
        *@return true if boxes intersect, otherwise false
        */
        virtual bool Intersect(const QR_CollideShape3D<M_Precision>* pOther) const;
};

#endif // QR_BoxH
