/*****************************************************************************
 * ==> QR_ViewController ----------------------------------------------------*
 *****************************************************************************
 * Description : Application view controller                                 *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#import "QR_ViewController.h"

// qr engine
#import "QR_Vector3D.h"
#import "QR_MD2.h"
#import "QR_MD2Animation.h"
#import "QR_Algorithms.h"
#import "QRX_SystemTools.h"
#import "QRX_StringTools.h"

//------------------------------------------------------------------------------
// QR_ViewController class private declarations
//------------------------------------------------------------------------------
@interface QR_ViewController()
{
    std::clock_t m_PrevTime;
}

@property (strong, nonatomic) EAGLContext* m_pContext;

/**
* Initializes OpenGL
*/
- (void)InitOpenGL;

/**
* Releases OpenGL
*/
- (void)ReleaseOpenGL;

/**
* Creates the scene
*/
- (void) CreateScene;

@end
//------------------------------------------------------------------------------
// QR_ViewController - objective c - Apple dependent
//------------------------------------------------------------------------------
@implementation QR_ViewController
//------------------------------------------------------------------------------
@synthesize m_pContext = _context;
//------------------------------------------------------------------------------
- (void)dealloc
{
    [_context release];

    [super dealloc];
}
//------------------------------------------------------------------------------
- (void)viewDidLoad
{
    [super viewDidLoad];

    // create OpenGL context. NOTE to use shaders, OpenGL ES2 or above must be used
    self.m_pContext = [[[EAGLContext alloc]initWithAPI:kEAGLRenderingAPIOpenGLES1]autorelease];

    if (!self.m_pContext)
        NSLog(@"Failed to create OpenGL ES context");

    // create render view
    GLKView* pView = (GLKView*)self.view;
    pView.context             = self.m_pContext;
    pView.drawableDepthFormat = GLKViewDrawableDepthFormat24;

    // initialize OpenGL
    [self InitOpenGL];

    // create scene
    [self CreateScene];
}
//------------------------------------------------------------------------------
- (void)viewDidUnload
{
    [super viewDidUnload];

    [self ReleaseOpenGL];

    if (m_pMD2Renderer)
        delete m_pMD2Renderer;

    if ([EAGLContext currentContext] == self.m_pContext)
        [EAGLContext setCurrentContext:nil];

    self.m_pContext = nil;
}
//------------------------------------------------------------------------------
- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
}
//------------------------------------------------------------------------------
- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    /*REM
    // get window width and height
    const float winWidth  = [[UIScreen mainScreen]bounds].size.width;
    const float winHeight = [[UIScreen mainScreen]bounds].size.height;

    // create viewport
    glViewport(0, 0, winWidth, winHeight);

    const float fov         = 45.0f;
    const float zNear       = 1.0f;
    const float zFar        = 1000.0f;
    const float aspectRatio = winWidth / winHeight;
    const float yMax        = zNear * tanf(fov * M_PI / 360.0);
    const float xMax        = yMax  * aspectRatio;
    
    // calculate and set projection matrix
    glMatrixMode(GL_PROJECTION);
    glFrustumf(-xMax, xMax, -yMax, yMax, zNear, zFar);
     */

    return NO;
}
//------------------------------------------------------------------------------
- (void) InitOpenGL
{
    [EAGLContext setCurrentContext:self.m_pContext];

    glEnable(GL_DEPTH_TEST);
}
//------------------------------------------------------------------------------
- (void) ReleaseOpenGL
{
    [EAGLContext setCurrentContext:self.m_pContext];
}
//------------------------------------------------------------------------------
- (void) CreateScene
{
    // initialize model
    m_pMD2Renderer              = new QR_MD2Renderer_OpenGL();
    QR_MD2*          pModel     = m_pMD2Renderer->GetModel();
    QR_MD2Animation* pAnimation = m_pMD2Renderer->GetAnimation();
    
    // found them?
    if (!pModel || !pAnimation)
        M_THROW_EXCEPTION("Model renderer is not initialized correctly");
    
    // get bundle resources directory
    NSString* pResourceDir = [QRX_SystemTools GetiOSResourcesDir];
    
    // get md2 file name
    NSString* pMD2FileName       = [QRX_StringTools StrToNSStr:"Female_animated.md2"];
    NSString* pMD2FileNameAndDir = [pResourceDir stringByAppendingPathComponent:pMD2FileName];

    // try to load mesh file
    if (pModel->Load([QRX_StringTools NSStrToStr:pMD2FileNameAndDir]) != QR_MD2Common::IE_C_Success)
        M_THROW_EXCEPTION("Unable to open mesh file");
    
    // get md2 normal table file name
    NSString* pTableFileName       = [QRX_StringTools StrToNSStr:"Normals.bin"];
    NSString* pTableFileNameAndDir = [pResourceDir stringByAppendingPathComponent:pTableFileName];

    // try to load mesh normals table
    if (pModel->LoadNormals([QRX_StringTools NSStrToStr:pTableFileNameAndDir]) != QR_MD2Common::IE_C_Success)
        M_THROW_EXCEPTION("Unable to open normals table file");

    // get window width and height
    const float winWidth  = [[UIScreen mainScreen]bounds].size.width;
    const float winHeight = [[UIScreen mainScreen]bounds].size.height;
    
    // create viewport
    glViewport(0, 0, winWidth, winHeight);
    
    const float fov         = 45.0f;
    const float zNear       = 1.0f;
    const float zFar        = 1000.0f;
    const float aspectRatio = winWidth / winHeight;
    const float yMax        = zNear * tanf(fov * M_PI / 360.0);
    const float xMax        = yMax  * aspectRatio;
    
    // calculate and set projection matrix
    glMatrixMode(GL_PROJECTION);
    glFrustumf(-xMax, xMax, -yMax, yMax, zNear, zFar);

    // set model color
    glColor4f(0.0f, 0.0f, 0.0f, 1.0f);

    pModel->SetTranslation(QR_Vector3DP(0.0f, -8.0f, -25.0f));
    pModel->SetRotationX(-QR_Algorithms::DegToRad(90.0f));
    pModel->SetRotationZ(-QR_Algorithms::DegToRad(45.0f));

    // configure mesh animation
    pAnimation->Add("WALK", 0, 39);
    pAnimation->Set("WALK");

    // initialize model renderer
    m_pMD2Renderer->Initialize();

    // initialize clock
    m_PrevTime = std::clock();
}
//------------------------------------------------------------------------------
- (void) update
{
    // calculate next frame
    std::clock_t now = std::clock();
    m_ElapsedTime    = (now - m_PrevTime) / 250;
    m_PrevTime       = now;
}
//------------------------------------------------------------------------------
- (void) glkView :(GLKView*)pView drawInRect:(CGRect)rect
{
    m_Renderer.BeginScene(QR_Color(240, 240, 240, 255),
                         (QR_Renderer::IESceneFlags)(QR_Renderer::IE_SF_ClearColor |
                                                     QR_Renderer::IE_SF_ClearDepth));

    // set vertex color
    glColor4f(0.0f, 0.0f, 0.0f, 1.0f);

    // draw mesh
    m_pMD2Renderer->Draw(m_ElapsedTime, 30);

    m_Renderer.EndScene();
}
//------------------------------------------------------------------------------
@end
//------------------------------------------------------------------------------
