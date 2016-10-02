/******************************************************************************
 * ==> QR_SimplePolygon ------------------------------------------------------*
 ******************************************************************************
 * Description : Polygon that contains vertex coordinates as real values      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_SimplePolygonH
#define QR_SimplePolygonH

// qr engine
#include "QR_Polygon.h"

/**
* Polygon that contains vertex coordinates as real values
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_SimplePolygon : public QR_Polygon
{
    public:
        /**
        * Constructor
        */
        QR_SimplePolygon();

        /**
        * Constructor
        *@param vertex1 - first vertex of the polygon
        *@param vertex2 - second vertex of the polygon
        *@param vertex3 - third vertex of the polygon
        */
        QR_SimplePolygon(const QR_Vector3DP& vertex1,
                         const QR_Vector3DP& vertex2,
                         const QR_Vector3DP& vertex3);

        /**
        * Destructor
        */
        virtual ~QR_SimplePolygon();

        /**
        * Gets vertex at index
        *@param index - vertex index
        *@return corresponding vertex, empty vertex if not found
        */
        virtual QR_Vector3DP GetVertex(QR_UInt8 index) const;

        /**
        * Sets vertex
        *@param index - vertex index to set
        *@param vertex - vertex value
        */
        virtual void SetVertex(QR_UInt8 index, const QR_Vector3DP& vertex);

        /**
        * Gets first polygon vertex
        *@return first polygon vertex
        */
        virtual QR_Vector3DP GetVertex1() const;

        /**
        * Sets first polygon vertex
        *@param value - first polygon vertex value
        */
        virtual void SetVertex1(const QR_Vector3DP& value);

        /**
        * Gets second polygon vertex
        *@return second polygon vertex
        */
        virtual QR_Vector3DP GetVertex2() const;

        /**
        * Sets second polygon vertex
        *@param value - second polygon vertex value
        */
        virtual void SetVertex2(const QR_Vector3DP& value);

        /**
        * Gets third polygon vertex
        *@return third polygon vertex
        */
        virtual QR_Vector3DP GetVertex3() const;

        /**
        * Sets third polygon vertex
        *@param value - third polygon vertex value
        */
        virtual void SetVertex3(const QR_Vector3DP& value);

        /**
        * Creates and returns a clone of the polygon
        *@return a clone of the polygon
        *@note The returned polygon should be deleted when useless
        */
        virtual QR_Polygon* GetClone() const;

        /**
        * Applies the given matrix to the polygon
        *@param matrix - matrix to apply
        *@return transformed polygon
        *@note The returned polygon should be deleted when useless
        */
        virtual QR_Polygon* ApplyMatrix(const QR_Matrix16P& matrix) const;

    private:
        QR_Vector3DP m_Vertex[3];
};

#endif // QR_SimplePolygonH
