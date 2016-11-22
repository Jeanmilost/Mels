/*****************************************************************************
 * ==> QR_Shader ------------------------------------------------------------*
 *****************************************************************************
 * Description : Basic shader language class                                 *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_ShaderH
#define QR_ShaderH

// std
#include <map>
#include <string>

// qr engine
#include "QR_Types.h"

/**
* Basic shader language class
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_Shader
{
    public:
        /**
        * Shader type enumeration
        */
        enum IEType
        {
            IE_ST_Vertex,
            IE_ST_Fragment,
        };

        /**
        * Shader attributes
        */
        enum IEAttribute
        {
            IE_SA_Position,
            IE_SA_Normal,
            IE_SA_Texture,
            IE_SA_Color,
            IE_SA_PerspectiveMatrix,
            IE_SA_ProjectionMatrix, // normally the same as perspective matrix, just for name convention
            IE_SA_ViewMatrix,
            IE_SA_CameraMatrix,     // normally the same as view matrix, just for name convention
            IE_SA_ModelMatrix,
            IE_SA_Interpolation,
            IE_SA_InterpolationPos,
            IE_SA_InterpolationNormal,
        };

        QR_Shader();
        virtual ~QR_Shader();

        /**
        * Gets attribute name
        *@param attribute - attribute to get name
        *@return attribute name
        */
        virtual std::string GetAttributeName(IEAttribute attribute) const;

        /**
        * Sets attribute name
        *@param attribute - attribute to set name
        *@param name - new attribute name
        */
        virtual void SetAttributeName(      IEAttribute  attribute,
                                      const std::string& name);

        /**
        * Gets shader program identifier
        *@return shader program identifier
        */
        virtual QR_UIntPtrT GetProgramID() const = 0;

        /**
        * Attaches shader to program from file
        *@param fileName - shader file
        *@param type - shader type
        *@return compiled shader identifier
        */
        virtual QR_UIntPtrT AttachFile(const std::string& fileName, IEType type) = 0;

        /**
        * Attaches shader to program
        *@param source - shader source code
        *@param type - shader type
        *@return compiled shader identifier
        */
        virtual QR_UIntPtrT Attach(const std::string& source, IEType type) = 0;

        /**
        * Links all attached shader and keep program ready to run
        *@param use - if true, program will be used immediately (in case link succeeded)
        *@return true on success, otherwise false
        */
        virtual bool Link(bool use) = 0;

        /**
        * Uses the program
        *@param use - if true, program will be used, released otherwise
        */
        virtual void Use(bool use) = 0;

    private:
        typedef std::map<IEAttribute, std::string> IAttributeDictionary;

        IAttributeDictionary m_AttributeDictionary;

        /**
        * Populates default attribute dictionary
        */
        void PopulateAttributeDict();
};

#endif // QR_ShaderH
