/******************************************************************************
 * ==> QR_Ray ----------------------------------------------------------------*
 ******************************************************************************
 * Description : Euclidean ray                                                *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_RayH
#define QR_RayH

// std
#include <limits>

// qr engine
#include "QR_Types.h"
#include "QR_Vector3D.h"

/**
* Euclidean ray
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_Ray
{
    public:
        QR_Vector3D<T> m_Pos;    // ray position
        QR_Vector3D<T> m_Dir;    // ray direction
        QR_Vector3D<T> m_InvDir; // invert of direction (i.e. 1 / dir)

        /**
        * Constructor
        */
        QR_Ray();

        /**
        * Constructor
        *@param pos - ray position
        *@param dir - ray direction
        */
        QR_Ray(const QR_Vector3D<T>& pos, const QR_Vector3D<T>& dir);

        /**
        * Destructor
        */
        virtual ~QR_Ray();
};

// formatted ray using global precision
typedef QR_Ray<M_Precision> QR_RayP;

//------------------------------------------------------------------------------
// QR_Matrix16 implementation - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_Ray<T>::QR_Ray() :
    m_InvDir(QR_Vector3D<T>(std::numeric_limits<double>::infinity(),
                            std::numeric_limits<double>::infinity(),
                            std::numeric_limits<double>::infinity()))
{}
//------------------------------------------------------------------------------
template <class T>
QR_Ray<T>::QR_Ray(const QR_Vector3D<T>& pos, const QR_Vector3D<T>& dir) :
    m_Pos(pos),
    m_Dir(dir),
    m_InvDir(QR_Vector3D<T>((dir.m_X == 0.0f) ? std::numeric_limits<double>::infinity() : (1.0f / dir.m_X),
                            (dir.m_Y == 0.0f) ? std::numeric_limits<double>::infinity() : (1.0f / dir.m_Y),
                            (dir.m_Z == 0.0f) ? std::numeric_limits<double>::infinity() : (1.0f / dir.m_Z)))
{}
//------------------------------------------------------------------------------
template <class T>
QR_Ray<T>::~QR_Ray()
{}
//------------------------------------------------------------------------------

#endif // QR_RayH
