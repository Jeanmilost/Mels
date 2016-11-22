/******************************************************************************
 * ==> QR_AppDelegate --------------------------------------------------------*
 ******************************************************************************
 * Description : Application delegate                                         *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#import <UIKit/UIKit.h>

@class QR_ViewController;

/**
* Application delegate
*@author Jean-Milost Reymond
*/
@interface QR_AppDelegate : UIResponder <UIApplicationDelegate>
{}

@property (strong, nonatomic) UIWindow*          m_pWindow;
@property (strong, nonatomic) QR_ViewController* m_pViewController;

@end
