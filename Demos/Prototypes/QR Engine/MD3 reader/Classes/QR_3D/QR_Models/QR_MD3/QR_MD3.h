/******************************************************************************
 * ==> QR_MD3 ----------------------------------------------------------------*
 ******************************************************************************
 * Description : MD3 model                                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3H
#define QR_MD3H

// std
#include <vector>
#include <map>

// qr engine
#include "QR_Types.h"
#include "QR_Color.h"
#include "QR_Vector3D.h"
#include "QR_Vertex.h"
#include "QR_MD3Parser.h"
#include "QR_FramedModel.h"

/**
* MD3 model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3 : public QR_FramedModel
{
    public:
        QR_MD3();
        virtual ~QR_MD3();

        /**
        * Loads MD3 from file
        *@param fileName - file name
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        */
        virtual QR_Int32 Load(const std::string&  fileName);
        virtual QR_Int32 Load(const std::wstring& fileName);

        /**
        * Loads MD3 from buffer
        *@param buffer - buffer
        *@param length - length to read in buffer, in bytes
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        *@note Read will begin from current offset
        */
        virtual QR_Int32 Load(      QR_Buffer&            buffer,
                              const QR_Buffer::ISizeType& length);

        /**
        * Gets MD3 parser
        *@return MD3 parser
        */
        const QR_MD3Parser* GetParser() const;

        /**
        * Sets color
        *@param color - color
        */
        virtual void SetColor(const QR_Color& color);

        /**
        * Gets mesh
        *@param index - mesh index to get
        *@param[out] mesh - mesh
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        *@note vertex buffer content is organized as follow:
        *      [1]x [2]y [3]z [4]nx [5]ny [6]nz [7]tu [8]tv [9]r [10]g [11]b [12]a
        *      where:
        *      x/y/z    - vertex coordinates
        *      nx/ny/nz - vertex normal (if includeNormal is activated)
        *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
        *      r/g/b/a  - vertex color(if includeColor is activated)
        *@note Mesh content should be deleted when useless. WARNING, the output mesh should
        *      be deleted even if function failed
        */
        virtual QR_Int32 GetMesh(QR_SizeT index, QR_Mesh& mesh) const;

        /**
        * Gets mesh count
        *@return mesh count
        */
        virtual QR_SizeT GetMeshCount() const;

        /**
        * Interpolates mesh
        *@param position - interpolation position, in percent (between 0.0f and 1.0f)
        *@param mesh1 - first mesh to interpolate
        *@param mesh2 - second mesh to interpolate
        *@param[out] result -resulting interpolated mesh
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        *@note This function should only be used for compatibility with old OpenGL 1.x versions, as
        *      normally interpolation should be done in vertex shader
        */
        static QR_Int32 Interpolate(const QR_Float& position,
                                    const QR_Mesh&  mesh1,
                                    const QR_Mesh&  mesh2,
                                          QR_Mesh&  result);

    protected:
        typedef std::vector<QR_Vector3DP>       IVectors3D;
        typedef std::vector<QR_TexCoord>        ITexCoords;
        typedef std::map<QR_SizeT, IVectors3D*> IVectorDictionary;
        typedef std::map<QR_SizeT, ITexCoords*> ITextureDictionary;

        /**
        * Prepares mesh to be used by mesh generator
        */
        void PrepareMesh();

        /**
        * Adds vertex to vertex buffer
        *@param index - mesh index to get
        *@param indice - vertex indice as read from md3 file
        *@param vertexFormat - vertex format to apply
        *@param vertices - vertices list read from md3 file
        *@param normals - normals list read from md3 file
        *@param texCoords - texture coordinate list read from md3 file
        *@param color - vertex color to apply
        *@param pVertex - vertex containing buffer to add to
        */
        void AddVertex(      QR_SizeT            index,
                             QR_SizeT            indice,
                             QR_Vertex::IEFormat vertexFormat,
                       const IVectors3D&         vertices,
                       const IVectors3D&         normals,
                       const ITexCoords&         texCoords,
                       const QR_Color&           color,
                             QR_Vertex*          pVertex) const;

        /**
        * Uncompresses normal
        *@param latitude - normal latitude as written if md3 file
        *@param longitude - normal longitude as written if md3 file
        *@return uncompressed normal
        */
        QR_Vector3DP UncompressNormal(QR_Int8 latitude, QR_Int8 longitude) const;

    private:
        QR_MD3Parser       m_Model;
        IVectorDictionary  m_Vertices;
        IVectorDictionary  m_Normals;
        ITextureDictionary m_TexCoords;
        QR_Color           m_Color;
};

#endif // QR_MD3H
