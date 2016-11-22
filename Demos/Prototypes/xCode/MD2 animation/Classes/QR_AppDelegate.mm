/******************************************************************************
 * ==> QR_AppDelegate --------------------------------------------------------*
 ******************************************************************************
 * Description : Application delegate                                         *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#import "QR_AppDelegate.h"

// interface
#import "QR_ViewController.h"

//------------------------------------------------------------------------------
// QR_AppDelegate - objective-c Apple dependent
//------------------------------------------------------------------------------
@implementation QR_AppDelegate
//------------------------------------------------------------------------------
@synthesize m_pWindow         = _window;
@synthesize m_pViewController = _viewController;
//------------------------------------------------------------------------------
- (void)dealloc
{
    [_window release];
    [_viewController release];
    [super dealloc];
}
//------------------------------------------------------------------------------
- (BOOL)application:(UIApplication*)pApplication didFinishLaunchingWithOptions:(NSDictionary*)pLaunchOptions
{
    self.m_pWindow = [[[UIWindow alloc] initWithFrame:[[UIScreen mainScreen]bounds]]autorelease];
    
    // override point for customization after application launch.
    self.m_pViewController = [[[QR_ViewController alloc] initWithNibName:@"QR_ViewController"
                                                                  bundle:nil]autorelease];
    self.m_pWindow.rootViewController = self.m_pViewController;
    [self.m_pWindow makeKeyAndVisible];
    
    return YES;
}
//------------------------------------------------------------------------------
- (void)applicationWillResignActive:(UIApplication*)pApplication
{}
//------------------------------------------------------------------------------
- (void)applicationDidEnterBackground:(UIApplication*)pApplication
{}
//------------------------------------------------------------------------------
- (void)applicationWillEnterForeground:(UIApplication*)pApplication
{}
//------------------------------------------------------------------------------
- (void)applicationDidBecomeActive:(UIApplication*)pApplication
{}
//------------------------------------------------------------------------------
- (void)applicationWillTerminate:(UIApplication*)pApplication
{}
//------------------------------------------------------------------------------
@end
//------------------------------------------------------------------------------
