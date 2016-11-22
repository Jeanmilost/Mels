/*****************************************************************************
 * ==> QRX_SystemTools ------------------------------------------------------*
 * ***************************************************************************
 * Description : OSX and iOS system tools                                    *
 * Developper  : Jean-Milost Reymond                                         *
 *****************************************************************************/

#import <Foundation/Foundation.h>

/**
* Mac OS X and iOS system tools
*@author Jean-Milost Reymond
*/
@interface QRX_SystemTools : NSObject
{}

/**
* Gets current application name
*@returns name
*/
+ (NSString*) GetAppName;

/**
* Gets current application version as string
*@returns version
*/
+ (NSString*) GetAppVersionStr;

/**
* Gets system name
*@returns name
*/
+ (NSString*) GetOSName;

/**
* Gets system version as human readable string
*@returns version
*@note Result contains OS version and build number
*/
+ (NSString*) GetOSVersionStr;

/**
* Gets total available memory as string
*@returns memory
*/
+ (NSString*) GetMemoryStr;

/**
* Checks if file exists
*@param pFileName - file name to check (including dir)
*@returns YES if file exists, otherwise NO
*/
+ (BOOL) FileExists :(NSString*)pFileName;

/**
* Checks if directory exists
*@param pDirName - directory name to check (including parent)
*@returns YES if directory exists, otherwise NO
*/
+ (BOOL) DirExists :(NSString*)pDirName;

/**
* Creates directory
*@param pDirName - directory name to create
*@param createIntermediate - whether or not intermediate dir should be created
*@returns YES on success or if directory already exists, otherwise NO
*/
+ (BOOL) CreateDir :(NSString*)pDirName :(BOOL)createIntermediate;

/**
* Count number of files contained in a directory
*@param pDirName - directory name to check
*@returns number of contained files
*/
+ (unsigned long) CountFiles :(NSString*)pDirName;

/**
* Get iOS document directory
*@returns directory name
*/
+ (NSString*) GetiOSDocumentDir;

/**
* Get iOS resources directory
*@returns directory name
*/
+ (NSString*) GetiOSResourcesDir;

/**
* Delete file
*@param pFileName - file name to delete
*@returns YES on success, otherwise NO
*/
+ (BOOL) DeleteFile :(NSString*)pFileName;

/**
* Gets IP address
*@returns IP address
*/
+ (NSString*)GetIPAddress;

/*
* Gets running platform
*@returns platform
*@note Platforms can be e.g.
*      - "i386"      on the simulator
*      - "iPod1,1"   on iPod Touch
*      - "iPod2,1"   on iPod Touch Second Generation
*      - "iPod3,1"   on iPod Touch Third Generation
*      - "iPod4,1"   on iPod Touch Fourth Generation
*      - "iPhone1,1" on iPhone
*      - "iPhone1,2" on iPhone 3G
*      - "iPhone2,1" on iPhone 3GS
*      - "iPad1,1"   on iPad
*      - "iPad2,1"   on iPad 2
*      - "iPhone3,1" on iPhone 4
*      - "iPhone4,1" on iPhone 4S
*        ...
*/
+ (NSString*)GetPlatform;

/**
* Generates UUID as string
*@returns UUID
*/
+ (NSString*) GenerateUUIDString;

@end
