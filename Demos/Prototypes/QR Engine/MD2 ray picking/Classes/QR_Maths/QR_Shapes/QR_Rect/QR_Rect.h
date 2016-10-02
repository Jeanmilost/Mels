/******************************************************************************
 * ==> QR_Rect ---------------------------------------------------------------*
 ******************************************************************************
 * Description : Generic rectangle, can be used e.g. to delimit 2D area, ...  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_RectH
#define QR_RectH

/**
* Generic rectangle, can be used e.g. to delimit 2D area, ...
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
template <class T>
class QR_Rect
{
    public:
        T m_Left;
        T m_Top;
        T m_Width;
        T m_Height;

        /**
        * Constructor
        */
        QR_Rect();

        /**
        * Constructor
        */
        QR_Rect(T Left, T top, T width, T height);

        /**
        * Destructor
        */
        virtual ~QR_Rect();
};

//------------------------------------------------------------------------------
// QR_Rect - c++ cross-platform
//------------------------------------------------------------------------------
template <class T>
QR_Rect<T>::QR_Rect() :
    m_Left(0),
    m_Top(0),
    m_Width(0),
    m_Height(0)
{}
//------------------------------------------------------------------------------
template <class T>
QR_Rect<T>::QR_Rect(T left, T top, T width, T height) :
    m_Left(left),
    m_Top(top),
    m_Width(width),
    m_Height(height)
{}
//------------------------------------------------------------------------------
template <class T>
QR_Rect<T>::~QR_Rect()
{}
//------------------------------------------------------------------------------

#endif // QR_RectH
