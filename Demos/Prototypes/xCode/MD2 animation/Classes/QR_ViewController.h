/******************************************************************************
 * ==> QR_ViewController -----------------------------------------------------*
 ******************************************************************************
 * Description : Application view controller                                  *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#import <UIKit/UIKit.h>
#import <GLKit/GLKit.h>

// qr engine
#include "QR_MD2Renderer_OpenGL.h"
#include "QR_Renderer_OpenGL.h"

/**
* View controller
*@author Jean-Milost Reymond
*/
@interface QR_ViewController : GLKViewController
{
    @private
        QR_Renderer_OpenGL     m_Renderer;
        QR_MD2Renderer_OpenGL* m_pMD2Renderer;
        std::clock_t           m_ElapsedTime;
}

@end
