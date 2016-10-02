/******************************************************************************
 * ==> QR_Line ---------------------------------------------------------------*
 ******************************************************************************
 * Description : Euclidean line                                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_LineH
#define QR_LineH

// std
#include <math.h>

// qr engine
#include "QR_Types.h"
#include "QR_Vector3D.h"

/**
* Line
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_Line
{
    public:
        QR_Vector3D<T> m_Start;
        QR_Vector3D<T> m_End;

        /**
        * Constructor
        */
        inline QR_Line();

        /**
        * Constructor
        *@param start . line start position
        *@param end - line end position
        */
        inline QR_Line(const QR_Vector3D<T>& start, const QR_Vector3D<T>& end);

        /**
        * Destructor
        */
        virtual inline ~QR_Line();

        /**
        * Calculates distance to line
        *@param point - point from which the distance must be calculated
        *@return distance to line
        */
        inline T DistanceTo(const QR_Vector3D<T>& point) const;

        /**
        * Get shortest distance between 2 lines
        *@param line - line to which distance must be calculated
        *@return shortest distance between lines
        */
        inline T GetShortestDistance(const QR_Line& line);
};

// formatted line using global precision
typedef QR_Line<M_Precision> QR_LineP;

//------------------------------------------------------------------------------
// QR_Line implementation - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_Line<T>::QR_Line()
{}
//------------------------------------------------------------------------------
template <class T>
QR_Line<T>::QR_Line(const QR_Vector3D<T>& start, const QR_Vector3D<T>& end)
{
    m_Start = start;
    m_End   = end;
}
//------------------------------------------------------------------------------
template <class T>
QR_Line<T>::~QR_Line()
{}
//------------------------------------------------------------------------------
template <class T>
T QR_Line<T>::DistanceTo(const QR_Vector3D<T>& point) const
{
    // calculate line delta on each axis
    const T xDelta = m_End.m_X - m_Start.m_X;
    const T yDelta = m_End.m_Y - m_Start.m_Y;
    const T zDelta = m_End.m_Z - m_Start.m_Z;

    // calculate projected point on line
    const T u = ((point.m_X - m_Start.m_X) * xDelta  +
                 (point.m_Y - m_Start.m_Y) * yDelta  +
                 (point.m_Z - m_Start.m_Z) * zDelta) /
                 ((xDelta * xDelta) + (yDelta * yDelta) + (zDelta * zDelta));

    QR_Vector3D<T> closestPointOnLine;

    // calculate closest point on line
    if (u < 0.0f)
        closestPointOnLine = m_Start;
    else
    if (u > 1.0f)
        closestPointOnLine = m_End;
    else
        closestPointOnLine = QR_Vector3D<T>(m_Start.m_X + u * xDelta,
                                            m_Start.m_Y + u * yDelta,
                                            m_Start.m_Z + u * zDelta);

    // compute distance between point and closest point on line
    QR_Vector3D<T> d = point - closestPointOnLine;

    // calculate distance
    return sqrt(d.m_X * d.m_X + d.m_Y * d.m_Y + d.m_Z * d.m_Z);
}
//------------------------------------------------------------------------------
template <class T>
T QR_Line<T>::GetShortestDistance(const QR_Line<T>& line)
{
    // calculate p2 - p1, p4 - p3, and p1 - p3 line segments
    QR_Vector3D<T> delta21 = m_End      - m_Start;
    QR_Vector3D<T> delta43 = line.m_End - line.m_Start;
    QR_Vector3D<T> delta13 = m_Start    - line.m_Start;

    // calculate some needed values - a, c and D are always >= 0
    T a = delta21.Dot(delta21);
    T b = delta21.Dot(delta43);
    T c = delta43.Dot(delta43);
    T d = delta21.Dot(delta13);
    T e = delta43.Dot(delta13);
    T D = a * c - b * b;

    // sc = sN / sD, default sD = D >= 0
    T sc, sN, sD = D;

    // tc = tN / tD, default tD = D >= 0
    T tc, tN, tD = D;

    // compute the line parameters of the two closest points
    if (D < M_Epsilon)
    {
        // the lines are almost parallel, force using point P0 on segment S1
        // to prevent possible division by 0 later
        sN = 0.0;
        sD = 1.0;
        tN = e;
        tD = c;
    }
    else
    {
        // get the closest points on the infinite lines
        sN = (b * e - c * d);
        tN = (a * e - b * d);

        // sc < 0 => the s=0 edge is visible
        if (sN < 0.0)
        {
            sN = 0.0;
            tN = e;
            tD = c;
        }
        else
        // sc > 1 => the s=1 edge is visible
        if (sN > sD)
        {
            sN = sD;
            tN = e + b;
            tD = c;
        }
    }

    // tc < 0 => the t=0 edge is visible
    if (tN < 0.0)
    {
        tN = 0.0;

        // recompute sc for this edge
        if (-d < 0.0)
            sN = 0.0;
        else
        if (-d > a)
            sN = sD;
        else
        {
            sN = -d;
            sD =  a;
        }
    }
    else
    // tc > 1 => the t=1 edge is visible
    if (tN > tD)
    {
        tN = tD;

        // recompute sc for this edge
        if ((-d + b) < 0.0)
            sN = 0;
        else
        if ((-d + b) > a)
            sN = sD;
        else
        {
            sN = (-d + b);
            sD = a;
        }
    }

    // finally do the division to get sc and tc
    if (fabs(sN) < M_Epsilon)
        sc = 0.0;
    else
        sc = sN / sD;

    if (fabs(tN) < M_Epsilon)
        tc = 0.0;
    else
        tc = tN / tD;

    QR_Vector3D<T> dP;

    // get the difference of the two closest points
    dP.m_X = delta13.m_X + (sc * delta21.m_X) - (tc * delta43.m_X);
    dP.m_Y = delta13.m_Y + (sc * delta21.m_Y) - (tc * delta43.m_Y);
    dP.m_Z = delta13.m_Z + (sc * delta21.m_Z) - (tc * delta43.m_Z);

    // return the closest distance
    return sqrt(dP.Dot(dP));
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

#endif // QR_LineH
