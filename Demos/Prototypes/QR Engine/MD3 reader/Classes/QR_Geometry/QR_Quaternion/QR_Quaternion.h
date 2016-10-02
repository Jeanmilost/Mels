/******************************************************************************
 * ==> QR_Quaternion ---------------------------------------------------------*
 ******************************************************************************
 * Description : Quaternion implementation                                    *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_QuaternionH
#define QR_QuaternionH

// std
#include <math.h>

// qr engine
#include "QR_Vector3D.h"
#include "QR_Matrix16.h"

// do use without others QR Engine classes?
#ifndef USE_QR_GEOMETRY_OLNY
    // qr engine
    #include "QR_Types.h"
    #include "QR_Exception.h"
#else
    #define M_THROW_EXCEPTION(message) throw message
    #define QR_Float                   float
    #define QR_Double                  double
#endif

/**
* Quaternion
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template<class T>
class QR_Quaternion
{
    public:
        T m_X;
        T m_Y;
        T m_Z;
        T m_W;

        /**
        * Constructor
        */
        inline QR_Quaternion();

        /**
        * Constructor
        *@param x - vector x value
        *@param y - vector y value
        *@param z - vector z value
        *@param w - vector w value
        */
        inline QR_Quaternion(T x, T y, T z, T w);

        /**
        * Constructor that creates a quaternion from an axis and an angle
        *@param vector - vector representing axis
        *@param angle - angle in radians
        */
        inline QR_Quaternion(const QR_Vector3D<T>& vector, T angle);

        /**
        * Constructor that creates a quaternion from a 4x4 matrix
        *@param matrix - matrix
        */
        QR_Quaternion(const QR_Matrix16<T>& matrix);

        /**
        * Copy constructor
        *@param other - other quaternion to copy from
        */
        inline QR_Quaternion(const QR_Quaternion& other);

        virtual inline ~QR_Quaternion();

        /**
        * Copy operator
        *@param other - other quaternion to copy from
        *@return this quaternion
        */
        virtual inline QR_Quaternion& operator =(const QR_Quaternion& other);

        /**
        * Operator + for single value
        *@param value - value to add
        *@return resulting added quaternion
        */
        virtual inline QR_Quaternion operator + (T value) const;

        /**
        * Operator +
        *@param other - other quaternion to add
        *@return resulting added quaternion
        */
        virtual inline QR_Quaternion operator + (const QR_Quaternion& other) const;

        /**
        * Operator -
        *@return inverted quaternion
        */
        virtual inline QR_Quaternion operator - () const;

        /**
        * Operator - for single value
        *@param value - value to subtract
        *@return resulting subtracted quaternion
        */
        virtual inline QR_Quaternion operator - (T value) const;

        /**
        * Operator -
        *@param other - other quaternion to subtract
        *@return resulting subtracted quaternion
        */
        virtual inline QR_Quaternion operator - (const QR_Quaternion& other) const;

        /**
        * Operator * for single value
        *@param value - value to multiply
        *@return resulting multiplied quaternion
        */
        virtual inline QR_Quaternion operator * (T value) const;

        /**
        * Operator *
        *@param other - other quaternion to multiply
        *@return resulting multiplied quaternion
        */
        virtual inline QR_Quaternion operator * (const QR_Quaternion& other) const;

        /**
        * Operator / for single value
        *@param value - value to divide
        *@return resulting divided quaternion
        */
        virtual inline QR_Quaternion operator / (T value) const;

        /**
        * Operator /
        *@param other - other quaternion to divide
        *@return resulting divided quaternion
        */
        virtual inline QR_Quaternion operator / (const QR_Quaternion& other) const;

        /**
        * Operator += for single value
        *@param value - value to add
        *@return resulting added quaternion
        */
        virtual inline const QR_Quaternion& operator += (T value);

        /**
        * Operator +=
        *@param other - other quaternion to add
        *@return resulting added quaternion
        */
        virtual inline const QR_Quaternion& operator += (const QR_Quaternion& other);

        /**
        * Operator -= for single value
        *@param value - value to subtract
        *@return resulting subtracted quaternion
        */
        virtual inline const QR_Quaternion& operator -= (T value);

        /**
        * Operator -=
        *@param other - other quaternion to subtract
        *@return resulting subtracted quaternion
        */
        virtual inline const QR_Quaternion& operator -= (const QR_Quaternion& other);

        /**
        * Operator *= for single value
        *@param value - value to multiply
        *@return resulting multiplied quaternion
        */
        virtual inline const QR_Quaternion& operator *= (T value);

        /**
        * Operator *=
        *@param other - other quaternion to multiply
        *@return resulting multiplied quaternion
        */
        virtual inline const QR_Quaternion& operator *= (const QR_Quaternion& other);

        /**
        * Operator /= for single value
        *@param value - value to divide
        *@return resulting divided quaternion
        */
        virtual inline const QR_Quaternion& operator /= (T value);

        /**
        * Operator /=
        *@param other - other quaternion to divide
        *@return resulting divided quaternion
        */
        virtual inline const QR_Quaternion& operator /= (const QR_Quaternion& other);

        /**
        * Operator ==
        *@param other - other quaternion to compare
        *@return true if quaternions are identical, otherwise false
        */
        virtual inline bool operator == (const QR_Quaternion& other) const;

        /**
        * Operator !=
        *@param other - other quaternion to compare
        *@return true if quaternions are not identical, otherwise false
        */
        virtual inline bool operator != (const QR_Quaternion& other) const;

        /**
        * Copies quaternion
        *@param other - other quaternion to copy from
        */
        virtual inline void Copy(const QR_Quaternion& other);

        /**
        * Calculates the norm of the quaternion
        *@return the norm of the quaternion
        */
        virtual inline T Norm() const;

        /**
        * Gets the quaternion length
        *@return the quaternion length
        */
        virtual inline T Length() const;

        /**
        * Normalizes the vector
        *@return normalized vector
        */
        virtual inline QR_Quaternion Normalize() const;

        /**
        * Calculates the dot product between 2 quaternions
        *@param q - quaternion with which dot product is calculated
        *@return dot product
        */
        virtual inline T Dot(const QR_Quaternion& q) const;

        /**
        * Scales the quaternion
        *@param s - scale factor to apply
        *@return scaled quaternion
        */
        virtual inline QR_Quaternion Scale(T s) const;

        /**
        * Conjugates quaternion
        *@return the conjugate of the quaternion
        */
        virtual inline QR_Quaternion Conjugate() const;

        /**
        * Inverse quaternion
        *@return inverted quaternion
        */
        virtual inline QR_Quaternion Inverse() const;

        /**
        * Gets the spherical linear interpolated quaternion between 2 quaternions
        *@param other - other quaternion to interpolate with
        *@param p - interpolation position, in percent (between 0.0f and 1.0f)
        *@return the spherical linear interpolated quaternion
        */
        virtual QR_Quaternion Slerp(const QR_Quaternion& other, T p) const;

        /**
        * Rotates a vector by the quaternion
        *@param vector - vector to rotate
        *@return rotated vector
        */
        virtual inline QR_Vector3D<T> Rotate(const QR_Vector3D<T>& vector) const;

        /**
        * Gets matrix from quaternion
        *@return matrix
        */
        virtual inline QR_Matrix16<T> GetMatrix() const;
};

#ifdef USE_QR_GEOMETRY_OLNY
    // formatted vector 3d using float or double
    typedef QR_Quaternion<QR_Float>  QR_QuaternionF;
    typedef QR_Quaternion<QR_Double> QR_QuaternionD;
#else
    // formatted vector 3d using global precision
    typedef QR_Quaternion<M_Precision> QR_QuaternionP;
#endif

//------------------------------------------------------------------------------
// QR_Quaternion - c++ cross-platform
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>::QR_Quaternion() :
    m_X(0.0f),
    m_Y(0.0f),
    m_Z(0.0f),
    m_W(1.0f)
{}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>::QR_Quaternion(T x, T y, T z, T w) :
    m_X(x),
    m_Y(y),
    m_Z(z),
    m_W(w)
{}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T>::QR_Quaternion(const QR_Vector3D<T>& vector, T angle)
{
    const T result = std::sinf(angle);

    m_X = (vector.m_X * result);
    m_Y = (vector.m_Y * result);
    m_Z = (vector.m_Z * result);
    m_W = std::cosf(angle);
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>::QR_Quaternion(const QR_Matrix16<T>& matrix)
{
    // calculate the matrix diagonal by adding up it's diagonal indices (also known as "trace")
    const T diagonal = matrix.m_Table[0][0] +
                       matrix.m_Table[1][1] +
                       matrix.m_Table[2][2] +
                       matrix.m_Table[3][3];

    T scale = 0.0f;

    // is diagonal greater than zero?
    if (diagonal > 0.00000001f)
    {
        // calculate the diagonal scale
        scale = sqrt(diagonal) * 2.0f;

        // calculate the quaternion values using the respective equation
        m_X = (matrix.m_Table[1][2] - matrix.m_Table[2][1]) / scale;
        m_Y = (matrix.m_Table[2][0] - matrix.m_Table[0][2]) / scale;
        m_Z = (matrix.m_Table[0][1] - matrix.m_Table[1][0]) / scale;
        m_W = 0.25f * scale;

        return;
    }

    // search for highest value in the matrix diagonal
    if (matrix.m_Table[0][0] > matrix.m_Table[1][1] && matrix.m_Table[0][0] > matrix.m_Table[2][2])
    {
        // calculate scale using the first diagonal element and double that value
        scale = sqrt(1.0f + matrix.m_Table[0][0] - matrix.m_Table[1][1] - matrix.m_Table[2][2]) * 2.0f;

        // calculate the quaternion values using the respective equation
        m_X = 0.25f * scale;
        m_Y = (matrix.m_Table[0][1] + matrix.m_Table[1][0]) / scale;
        m_Z = (matrix.m_Table[2][0] + matrix.m_Table[0][2]) / scale;
        m_W = (matrix.m_Table[1][2] - matrix.m_Table[2][1]) / scale;
    }
    else
    if (matrix.m_Table[1][1] > matrix.m_Table[2][2])
    {
        // calculate scale using the second diagonal element and double that value
        scale = sqrt(1.0f + matrix.m_Table[1][1] - matrix.m_Table[0][0] - matrix.m_Table[2][2]) * 2.0f;

        // calculate the quaternion values using the respective equation
        m_X = (matrix.m_Table[0][1] + matrix.m_Table[1][0]) / scale;
        m_Y = 0.25f * scale;
        m_Z = (matrix.m_Table[1][2] + matrix.m_Table[2][1]) / scale;
        m_W = (matrix.m_Table[2][0] - matrix.m_Table[0][2]) / scale;
    }
    else
    {
        // calculate scale using the third diagonal element and double that value
        scale = sqrt(1.0f + matrix.m_Table[2][2] - matrix.m_Table[0][0] - matrix.m_Table[1][1]) * 2.0f;

        // calculate the quaternion values using the respective equation
        m_X = (matrix.m_Table[2][0] + matrix.m_Table[0][2]) / scale;
        m_Y = (matrix.m_Table[1][2] + matrix.m_Table[2][1]) / scale;
        m_Z = 0.25f * scale;
        m_W = (matrix.m_Table[0][1] - matrix.m_Table[1][0]) / scale;
    }
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>::QR_Quaternion(const QR_Quaternion& other)
{
    Copy(other);
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>::~QR_Quaternion()
{}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T>& QR_Quaternion<T>::operator =(const QR_Quaternion& other)
{
    Copy(other);
    return *this;
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator + (T value) const
{
    return QR_Quaternion<T>(m_X + value, m_Y + value, m_Z + value, m_W + value);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator + (const QR_Quaternion& other) const
{
    return QR_Quaternion<T>(m_X + other.m_X, m_Y + other.m_Y, m_Z + other.m_Z, m_W + other.m_W);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator - () const
{
    return QR_Quaternion<T>(-m_X, -m_Y, -m_Z, -m_W);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator - (T value) const
{
    return QR_Quaternion<T>(m_X - value, m_Y - value, m_Z - value, m_W - value);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator - (const QR_Quaternion& other) const
{
    return QR_Quaternion<T>(m_X - other.m_X, m_Y - other.m_Y, m_Z - other.m_Z, m_W - other.m_W);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator * (T value) const
{
    return QR_Quaternion<T>(m_X * value, m_Y * value, m_Z * value, m_W * value);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator * (const QR_Quaternion& other) const
{
    return QR_Quaternion<T>(m_X * other.m_X, m_Y * other.m_Y, m_Z * other.m_Z, m_W * other.m_W);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator / (T value) const
{
    if (value == 0.0f)
        M_THROW_EXCEPTION("Division by 0 is prohibited");

    return QR_Quaternion<T>(m_X / value, m_Y / value, m_Z / value, m_W / value);
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::operator / (const QR_Quaternion& other) const
{
    if (other.m_X == 0.0f)
        M_THROW_EXCEPTION("Quaternion x value - division by 0 is prohibited");

    if (other.m_Y == 0.0f)
        M_THROW_EXCEPTION("Quaternion y value - division by 0 is prohibited");

    if (other.m_Z == 0.0f)
        M_THROW_EXCEPTION("Quaternion z value - division by 0 is prohibited");

    if (other.m_W == 0.0f)
        M_THROW_EXCEPTION("Quaternion z value - division by 0 is prohibited");

    return QR_Quaternion<T>(m_X / other.m_X, m_Y / other.m_Y, m_Z / other.m_Z, m_W / other.m_W);
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator += (T value)
{
    m_X += value;
    m_Y += value;
    m_Z += value;
    m_W += value;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator += (const QR_Quaternion& other)
{
    m_X += other.m_X;
    m_Y += other.m_Y;
    m_Z += other.m_Z;
    m_W += other.m_W;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator -= (T value)
{
    m_X -= value;
    m_Y -= value;
    m_Z -= value;
    m_W -= value;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator -= (const QR_Quaternion& other)
{
    m_X -= other.m_X;
    m_Y -= other.m_Y;
    m_Z -= other.m_Z;
    m_W -= other.m_W;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator *= (T value)
{
    m_X *= value;
    m_Y *= value;
    m_Z *= value;
    m_W *= value;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator *= (const QR_Quaternion& other)
{
    m_X *= other.m_X;
    m_Y *= other.m_Y;
    m_Z *= other.m_Z;
    m_W *= other.m_W;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator /= (T value)
{
    if (value == 0.0f)
        M_THROW_EXCEPTION("Division by 0 is prohibited");

    m_X /= value;
    m_Y /= value;
    m_Z /= value;
    m_W /= value;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
const QR_Quaternion<T>& QR_Quaternion<T>::operator /= (const QR_Quaternion& other)
{
    if (other.m_X == 0.0f)
        M_THROW_EXCEPTION("Quaternion x value - division by 0 is prohibited");

    if (other.m_Y == 0.0f)
        M_THROW_EXCEPTION("Quaternion y value - division by 0 is prohibited");

    if (other.m_Z == 0.0f)
        M_THROW_EXCEPTION("Quaternion z value - division by 0 is prohibited");

    if (other.m_W == 0.0f)
        M_THROW_EXCEPTION("Quaternion w value - division by 0 is prohibited");

    m_X /= other.m_X;
    m_Y /= other.m_Y;
    m_Z /= other.m_Z;
    m_W /= other.m_W;

    return *this;
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Quaternion<T>::operator == (const QR_Quaternion& other) const
{
    return ((m_X == other.m_X) && (m_Y == other.m_Y) && (m_Z == other.m_Z) && (m_W == other.m_W));
}
//------------------------------------------------------------------------------
template <class T>
bool QR_Quaternion<T>::operator != (const QR_Quaternion& other) const
{
    return ((m_X != other.m_X) || (m_Y != other.m_Y) || (m_Z != other.m_Z) || (m_W != other.m_W));
}
//------------------------------------------------------------------------------
template <class T>
void QR_Quaternion<T>::Copy(const QR_Quaternion& other)
{
    m_X = other.m_X;
    m_Y = other.m_Y;
    m_Z = other.m_Z;
    m_W = other.m_W;
}
//------------------------------------------------------------------------------
template <class T>
T QR_Quaternion<T>::Norm() const
{
    return ((m_X * m_X) + (m_Y * m_Y) + (m_Z * m_Z) + (m_W * m_W));
}
//------------------------------------------------------------------------------
template <class T>
T QR_Quaternion<T>::Length() const
{
    return sqrt(Norm());
}
//------------------------------------------------------------------------------
template <class T>
QR_Quaternion<T> QR_Quaternion<T>::Normalize() const
{
    T len = Length();

    if (len == 0.0f)
        return QR_Quaternion<T>();

    return QR_Quaternion<T>((m_X / len), (m_Y / len), (m_Z / len), (m_W / len));
}
//------------------------------------------------------------------------------
template <class T>
T QR_Quaternion<T>::Dot(const QR_Quaternion& q) const
{
    return ((m_X * q.m_X) + (m_Y * q.m_Y) + (m_Z * q.m_Z) + (m_W * q.m_W));
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T> QR_Quaternion<T>::Scale(T s) const
{
    return QR_Quaternion<T>(m_X * s, m_Y * s, m_Z * s, m_W * s);
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T> QR_Quaternion<T>::Conjugate() const
{
    return QR_Quaternion<T>(-m_X, -m_Y, -m_Z, m_W);
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T> QR_Quaternion<T>::Inverse() const
{
    // calculate the norm of the quaternion
    const T norm = Norm();

    // empty quaternion?
    if (!norm)
        return QR_Quaternion<T>();

    return Conjugate().Scale(1.0f / norm);
}
//------------------------------------------------------------------------------
template<class T>
QR_Quaternion<T> QR_Quaternion<T>::Slerp(const QR_Quaternion& other, T p) const
{
    // are quaternions identical?
    if ((*this) == other)
        return (*this);

    // calculate dot product between q1 and q2
    T result = Dot(other);

    QR_Quaternion interpolateWith;

    // check if angle is higher than 90° (this happen if dot product is less than 0)
    if (result < 0.0f)
    {
        // negate the second quaternion and the dot product result
        interpolateWith = -other;
        result          = -result;
    }
    else
        interpolateWith = other;

    // calculate the interpolation first and second scale
    T scale0 = 1.0f - p;
    T scale1 = p;

    // is angle large enough to apply the calculation
    if ((1.0f - result) > 0.1f)
    {
        // calculate the angle between the 2 quaternions and get the sinus of that angle
        T theta    = std::acos(result);
        T sinTheta = std::sinf(theta);

        // is resulting sinus equal to 0? (just to verify, should not happen)
        if (sinTheta == 0.0f)
            M_THROW_EXCEPTION("Invalid value");

        // calculate the scaling for q1 and q2, according to the angle and it's sine value
        scale0 = std::sinf((1.0f - p) * theta)  / sinTheta;
        scale1 = std::sinf((p         * theta)) / sinTheta;
    }

    QR_Quaternion interpolated;

    // calculate the resulting quaternion by using a special form of linear interpolation
    interpolated.m_X = (scale0 * m_X) + (scale1 * interpolateWith.m_X);
    interpolated.m_Y = (scale0 * m_Y) + (scale1 * interpolateWith.m_Y);
    interpolated.m_Z = (scale0 * m_Z) + (scale1 * interpolateWith.m_Z);
    interpolated.m_W = (scale0 * m_W) + (scale1 * interpolateWith.m_W);

    return interpolated;
}
//------------------------------------------------------------------------------
template<class T>
QR_Vector3D<T> QR_Quaternion<T>::Rotate(const QR_Vector3D<T>& vector) const
{
    // rotate vector
    QR_Quaternion qv(vector.m_X, vector.m_Y, vector.m_Z, 0);
    QR_Quaternion qm = (*this) * qv * Inverse();

    QR_Vector3D<T> result;
    result.m_X = qm.m_X;
    result.m_Y = qm.m_Y;
    result.m_Z = qm.m_Z;

    return result;
}
//------------------------------------------------------------------------------
template <class T>
QR_Matrix16<T> QR_Quaternion<T>::GetMatrix() const
{
    return QR_Matrix16<T>(1.0f - 2.0f * (m_Y * m_Y + m_Z * m_Z), 2.0f * (m_X * m_Y - m_W * m_Z),        2.0f * (m_X * m_Z + m_W * m_Y),        0.0f,
                          2.0f * (m_X * m_Y + m_W * m_Z),        1.0f - 2.0f * (m_X * m_X + m_Z * m_Z), 2.0f * (m_Y * m_Z - m_W * m_X),        0.0f,
                          2.0f * (m_X * m_Z - m_W * m_Y),        2.0f * (m_Y * m_Z + m_W * m_X),        1.0f - 2.0f * (m_X * m_X + m_Y * m_Y), 0.0f,
                          0.0f,                                  0.0f,                                  0.0f,                                  1.0f);
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

#endif // QR_QuaternionH
