/******************************************************************************
 * ==> QR_MD2Normals ---------------------------------------------------------*
 ******************************************************************************
 * Description : Pre-calculated normals table for MD2 files                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD2NormalsH
#define QR_MD2NormalsH

// qr engine
#include "QR_Vector3D.h"

//------------------------------------------------------------------------------
// Global defines
//------------------------------------------------------------------------------
#define M_MD2_NormalCount 162
//------------------------------------------------------------------------------

/**
* MD2 pre-calculated normals table
*@note This table is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD2Normals
{
    public:
        static QR_Vector3DP m_Table[M_MD2_NormalCount];
};

#endif // QR_MD2NormalsH
