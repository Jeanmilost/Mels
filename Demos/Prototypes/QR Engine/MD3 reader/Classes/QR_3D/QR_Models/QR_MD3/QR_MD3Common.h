/******************************************************************************
 * ==> QR_MD3Common ----------------------------------------------------------*
 ******************************************************************************
 * Description : Common classes, enums and values for MD3 model               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3CommonH
#define QR_MD3CommonH

//------------------------------------------------------------------------------
// Global defines
//------------------------------------------------------------------------------
#define M_MD3_Invalid_Index (QR_SizeT)-1
//------------------------------------------------------------------------------

/**
* Common classes, enums and values for MD3 model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3Common
{
    public:
        /**
        * Error, success and warning codes
        */
        enum IECode
        {
            IE_C_Not3DCoords          = -9,
            IE_C_IncompatibleFrames   = -8,
            IE_C_IncompatibleVertices = -7,
            IE_C_VertexNotFound       = -6,
            IE_C_VertexTableNotFound  = -5,
            IE_C_NormalTableNotFound  = -4,
            IE_C_TextureTableNotFound = -3,
            IE_C_IndexOutOfBounds     = -2,
            IE_C_InvalidFileVersion   = -1,
            IE_C_Success              =  0,
            IE_C_EmptyBuffer          =  1,
        };
};

#endif // QR_MD3CommonH
