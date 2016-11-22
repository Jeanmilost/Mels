/******************************************************************************
 * ==> QR_IndexedPolygon -----------------------------------------------------*
 ******************************************************************************
 * Description : Polygon connected to a vertex buffer and that can extract    *
 *               data from it directly                                        *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_IndexedPolygonH
#define QR_IndexedPolygonH

// qr engine
#include "QR_Polygon.h"
#include "QR_Vertex.h"

/**
* Polygon connected to a vertex buffer and that can extract data from it directly
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_IndexedPolygon : public QR_Polygon
{
    public:
        /**
        * Constructor
        */
        QR_IndexedPolygon();

        /**
        * Constructor
        *@param v1i - first polygon vertex index
        *@param v2i - second polygon vertex index
        *@param v3i - third polygon vertex index
        *@param pVertexBuffer - source vertex buffer to link with
        */
        QR_IndexedPolygon(      QR_SizeT            v1i,
                                QR_SizeT            v2i,
                                QR_SizeT            v3i,
                          const QR_Vertex::IBuffer* pVertexBuffer);

        /**
        * Destructor
        */
        virtual ~QR_IndexedPolygon();

        /**
        * Sets vertex info
        *@param v1i - first polygon vertex index
        *@param v2i - second polygon vertex index
        *@param v3i - third polygon vertex index
        *@param pVertexBuffer - source vertex buffer to link with
        */
        virtual void SetVertexInfo(      QR_SizeT            v1i,
                                         QR_SizeT            v2i,
                                         QR_SizeT            v3i,
                                   const QR_Vertex::IBuffer* pVertexBuffer);

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
        * Gets polygon raw data, i.e. indexes and linked vertex buffer
        *@param[out] i1 - first polygon index
        *@param[out] i2 - second polygon index
        *@param[out] i3 - third polygon index
        *@param[out] pBuffer - linked buffer
        */
        virtual void GetData(      QR_SizeT&            i1,
                                   QR_SizeT&            i2,
                                   QR_SizeT&            i3,
                             const QR_Vertex::IBuffer*& pBuffer) const;

    private:
        const QR_Vertex::IBuffer* m_pVertexBuffer;
              QR_SizeT            m_Index[3];
};

#endif // QR_BufferedPolygonH
