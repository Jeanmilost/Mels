/******************************************************************************
 * ==> QR_Plane --------------------------------------------------------------*
 ******************************************************************************
 * Description : Euclidean plane                                              *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_PlaneH
#define QR_PlaneH

// qr engine
#include "QR_Vector3D.h"

// do use without others QR Engine classes?
#ifndef USE_QR_GEOMETRY_OLNY
    #include "QR_Types.h"
#endif

/**
* Plane
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_Plane
{
    public:
        T m_A; // a coordinate for the plane
        T m_B; // b coordinate for the plane
        T m_C; // c coordinate for the plane
        T m_D; // d coordinate for the plane

        /**
        * Constructor
        */
        inline QR_Plane();

        /**
        * Constructor
        *@param a - a coordinate
        *@param b - b coordinate
        *@param c - c coordinate
        *@param d - d coordinate
        */
        inline QR_Plane(const T& a, const T& b, const T& c, const T& d);

        /**
        * Copy constructor
        *@param other - other matrix to copy
        */
        inline QR_Plane(const QR_Plane& other);

        /**
        * Destructor
        */
        inline ~QR_Plane();

        /**
        * Assignation operator
        *@param other - other matrix to copy from
        */
        inline QR_Plane& operator =(const QR_Plane& other);

        /**
        * Operator -
        *@return inverted plane
        */
        inline QR_Plane operator - () const;

        /**
        * Operator ==
        *@param other - other plane to compare
        *@return true if planes are identical, otherwise false
        */
        inline bool operator == (const QR_Plane& other) const;

        /**
        * Operator !=
        *@param other - other plane to compare
        *@return true if planes are not identical, otherwise false
        */
        inline bool operator != (const QR_Plane& other) const;

        /**
        * Copies plane from another
        *@param other - other matrix to copy from
        */
        inline void Copy(const QR_Plane& other);

        /**
        * Calculates a plane using 3 vertex
        *@param v1 - value of the first vertex
        *@param v2 - value of the second vertex
        *@param v3 - value of the thrid vertex
        *@return the built plane
        */
        static QR_Plane FromPoints(const QR_Vector3D<T>& v1,
                                   const QR_Vector3D<T>& v2,
                                   const QR_Vector3D<T>& v3);

        /**
        * Calculates a plane using a point and a normal
        *@param point - a point belongs to the plane
        *@param normal - normal of the plane
        *@return the built plane
        */
        static QR_Plane FromPointNormal(const QR_Vector3D<T>& point,
                                        const QR_Vector3D<T>& normal);

        /**
        * Calculates distance to plane
        *@param point - point from which the distance must be calculated
        *@return distance to plane
        */
        inline T DistanceTo(const QR_Vector3D<T>& point) const;

        /**
        * Checks if plane intersects line and calculates intersection point
        *@param v1 - line start
        *@param v2 - line end
        *@param[out] p - calculated point on plane
        *@return true if plane intersects line, otherwise false
        */
        inline bool IntersectLine(const QR_Vector3D<T>& v1,
                                  const QR_Vector3D<T>& v2,
                                        QR_Vector3D<T>& p) const;

        /**
        * Checks if plane intersects ray and calculates intersection point
        *@param rp - ray start point
        *@param rd - ray direction
        *@param[out] p - calculated point on plane
        *@return true if plane intersects ray, otherwise false
        */
        inline bool IntersectRay(const QR_Vector3D<T>& rp,
                                 const QR_Vector3D<T>& rd,
                                       QR_Vector3D<T>& p) const;

        /**
        * Compare plane with the given plane using the given tolerance
        *@param other - other plane to compare
        *@param tolerance - tolerance for comparison
        *@return true if planes are equals in the limits of the given tolerance,
        *         otherwise false
        */
        inline bool Compare(const QR_Plane& other,
                            const T&        tolerance) const;
};

#ifdef USE_QR_GEOMETRY_OLNY
    // formatted plane using float or double
    typedef QR_Plane<QR_Float>  QR_PlaneF;
    typedef QR_Plane<QR_Double> QR_PlaneD;
#else
    // formatted plane using global precision
    typedef QR_Plane<M_Precision> QR_PlaneP;
#endif

//------------------------------------------------------------------------------
// QR_Plane implementation - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T>::QR_Plane() :
    m_A(0),
    m_B(0),
    m_C(0),
    m_D(0)
{}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T>::QR_Plane(const T& a, const T& b, const T& c, const T& d) :
    m_A(a),
    m_B(b),
    m_C(c),
    m_D(d)
{}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T>::QR_Plane(const QR_Plane& other)
{
    Copy(other);
}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T>::~QR_Plane()
{}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T>& QR_Plane<T>::operator =(const QR_Plane& other)
{
    Copy(other);
    return *this;
}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T> QR_Plane<T>::operator - () const
{
    return QR_Plane<T>(-m_A, -m_B, -m_C, -m_D);
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Plane<T>::operator == (const QR_Plane<T>& other) const
{
    return ((m_A == other.m_A) &&
            (m_B == other.m_B) &&
            (m_C == other.m_C) &&
            (m_D == other.m_D));
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Plane<T>::operator != (const QR_Plane<T>& other) const
{
    return ((m_A != other.m_A) ||
            (m_B != other.m_B) ||
            (m_C != other.m_C) ||
            (m_D != other.m_D));
}
//------------------------------------------------------------------------------
template <class T>
void QR_Plane<T>::Copy(const QR_Plane& other)
{
    m_A = other.m_A;
    m_B = other.m_B;
    m_C = other.m_C;
    m_D = other.m_D;
}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T> QR_Plane<T>::FromPoints(const QR_Vector3D<T>& v1,
                                    const QR_Vector3D<T>& v2,
                                    const QR_Vector3D<T>& v3)
{
    // calculate edge vectors
    QR_Vector3D<T> e1 = v2 - v1;
    QR_Vector3D<T> e2 = v3 - v1;

    // calculate the normal of the plane
    QR_Vector3D<T> normal = e1.Cross(e2).Normalize();

    // calculate and return the plane
    return FromPointNormal(v1, normal);
}
//------------------------------------------------------------------------------
template <class T>
QR_Plane<T> QR_Plane<T>::FromPointNormal(const QR_Vector3D<T>& point,
                                         const QR_Vector3D<T>& normal)
{
    // the a, b, and c components are only the normal of the plane, and the D
    // component can be calculated using the aX + bY + cZ + d = 0 algorithm
    return QR_Plane<T>(normal.m_X,
                       normal.m_Y,
                       normal.m_Z,
                     -(normal.Dot(point)));
}
//------------------------------------------------------------------------------
template <class T>
T QR_Plane<T>::DistanceTo(const QR_Vector3D<T>& point) const
{
    // get the normal of the plane
    QR_Vector3D<T> n(m_A, m_B, m_C);

    // calculate the distance between the plane and the point
    return n.Dot(point) + m_D;
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Plane<T>::IntersectLine(const QR_Vector3D<T>& v1,
                                const QR_Vector3D<T>& v2,
                                      QR_Vector3D<T>& p) const
{
    // calculates the direction of the line
    const QR_Vector3D<T> direction(v2 - v1);

    return IntersectRay(v1, direction.Normalize(), p);
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Plane<T>::IntersectRay(const QR_Vector3D<T>& rp,
                               const QR_Vector3D<T>& rd,
                                     QR_Vector3D<T>& p) const
{
    // gets the normal of the plane
    const QR_Vector3D<T> normal(m_A, m_B, m_C);

    // calculates the angle between the line and the normal to the plane
    const T dot = normal.Dot(rd);

    // if normal to the plane is perpendicular to the line, then the line is
    // either parallel to the plane and there are no solutions or the line is
    // on the plane in which case there are an infinite number of solutions
    if (!dot)
        return false;

    const T temp = (m_D + normal.Dot(rp)) / dot;

    // calculates the intersection point
    p = QR_Vector3D<T>(rp.m_X - (temp * rd.m_X),
                       rp.m_Y - (temp * rd.m_Y),
                       rp.m_Z - (temp * rd.m_Z));

    return true;
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Plane<T>::Compare(const QR_Plane<T>& other,
                          const T&           tolerance) const
{
    return (((m_A >= (other.m_A - tolerance))  &&
             (m_A <= (other.m_A + tolerance))) &&
            ((m_B >= (other.m_B - tolerance))  &&
             (m_B <= (other.m_B + tolerance))) &&
            ((m_C >= (other.m_C - tolerance))  &&
             (m_C <= (other.m_C + tolerance))) &&
            ((m_D >= (other.m_D - tolerance))  &&
             (m_D <= (other.m_D + tolerance))));
}
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// RAD studio
//------------------------------------------------------------------------------
#ifdef __CODEGEARC__
    // needed to avoid the W8058 error "Cannot create pre-compiled header: header incomplete" warning in BCC compilers
    ;
#endif
//------------------------------------------------------------------------------

#endif // QR_PlaneH
