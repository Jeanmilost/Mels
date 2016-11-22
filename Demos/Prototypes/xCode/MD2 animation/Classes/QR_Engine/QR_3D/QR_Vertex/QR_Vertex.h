/******************************************************************************
 * ==> QR_Vertex -------------------------------------------------------------*
 ******************************************************************************
 * Description : Vertex descriptor, contains global enumeration and types     *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_VertexH
#define QR_VertexH

// qr engine
#include "QR_Types.h"

/**
* Vertex descriptor, contains global enumeration and types
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Vertex
{
    public:
        /**
        * Vertex buffer type enumeration
        */
        enum IEType
        {
            IE_VT_Unknown = 0,
            IE_VT_Triangles,
            IE_VT_TriangleStrip,
            IE_VT_TriangleFan,
            IE_VT_Quads,
            IE_VT_QuadStrip,
        };

        /**
        * Vertex format enumeration
        *@note Flags can be combined
        */
        enum IEFormat
        {
            IE_VF_None      = 0x00,
            IE_VF_Normals   = 0x01,
            IE_VF_TexCoords = 0x02,
            IE_VF_Colors    = 0x04,
        };

        /**
        * Vertex coordinate type
        */
        enum IECoordType
        {
            IE_VC_Unknown = 0,
            IE_VC_XY,
            IE_VC_XYZ,
        };

        typedef std::vector<M_Precision> IBuffer;

        QR_SizeT    m_Stride;    // vertex stride (i.e. length between each vertex) in bytes
        IEType      m_Type;      // vertex type (i.e. how vertex is organized: triangle list, triangle fan, ...)
        IEFormat    m_Format;    // vertex format (i.e. what data vertex contains: position, normal, texture, ...)
        IECoordType m_CoordType; // vertex coordinate type (i.e. 2D coordinates, 3D coordinates, ...)
        IBuffer     m_Buffer;    // vertex buffer

        QR_Vertex();
        virtual ~QR_Vertex();

        /**
        * Clones vertex in such manner that vertex info are copied, but not vertex buffer
        *@return cloned vertex
        *@note Cloned vertex should be deleted when useless
        */
        virtual QR_Vertex* Clone() const;

        /**
        * Calculates vertex stride
        *@return vertex stride
        */
        virtual QR_SizeT CalculateStride() const;

        /**
        * Compares vertex and determine if their format are equivalent
        *@param other - other vertex to compare with
        *@return true if both vertex header are equivalent, otherwise false
        */
        virtual bool CompareFormat(const QR_Vertex& other) const;

        /**
        * Verifies buffer alignment
        *@return true if buffer is correctly aligned, otherwise false
        */
        static bool VerifyAlignment();
};

/**
* Mesh, it's a set of vertex buffers representing a model
*@author Jean-Milost Reymond
*/
typedef std::vector<QR_Vertex*> QR_Mesh;

/**
* Indexes, used to get vertices from faces (e.g. MD3 files contain a list of faces from which
* vertices are get)
*@author Jean-Milost Reymond
*/
typedef std::vector<QR_SizeT> QR_Indexes;

/**
* Vertex index buffer
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_IndexBuffer
{
    public:
        QR_Vertex* m_pVertex;
        QR_Indexes m_Indexes;

        /**
        * Constructor
        */
        QR_IndexBuffer();

        /**
        * Constructor
        *@param pVertex - vertex containing source buffer
        */
        QR_IndexBuffer(QR_Vertex* pVertex);

        /**
        * Destructor
        */
        virtual ~QR_IndexBuffer();

        /**
        * Populates index buffer from vertex buffer
        *@param pVertex - vertex containing source buffer
        */
        virtual void Populate(QR_Vertex* pVertex);
};

/**
* Structure containing texture coordinates
*@author Jean-Milost Reymond
*/
struct QR_TexCoord
{
    M_Precision m_U;
    M_Precision m_V;

    /**
    * Constructor
    */
    QR_TexCoord();

    /**
    * Constructor
    *@param u - texture u coordinate
    *@param v - texture v coordinate
    */
    QR_TexCoord(M_Precision u, M_Precision v);

    /**
    * Destructor
    */
    virtual ~QR_TexCoord();
};

/**
* Texture coordinates
* vertices are get)
*@author Jean-Milost Reymond
*/
typedef std::vector<QR_TexCoord*> QR_TexCoords;

#endif // QR_VertexH
