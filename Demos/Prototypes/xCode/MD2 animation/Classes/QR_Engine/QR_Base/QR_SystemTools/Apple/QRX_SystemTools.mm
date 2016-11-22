/*****************************************************************************
 * ==> QRX_SystemTools ------------------------------------------------------*
 * ***************************************************************************
 * Description : OSX and iOS system tools                                    *
 * Developper  : Jean-Milost Reymond                                         *
 *****************************************************************************/

#import "QRX_SystemTools.h"
#import "QRX_StringTools.h"
#include <ifaddrs.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/sysctl.h>
#include "QR_DateAndTimeTools.h"

#ifdef M_USE_LOGGER
    #include "QR_Logger_HTML.h"
#endif

//@implementation UIDevice (Hardware)

//------------------------------------------------------------------------------
// class QRX_System_Tools - objective-c Apple dependent
//------------------------------------------------------------------------------
@implementation QRX_SystemTools
//------------------------------------------------------------------------------
+ (NSString*) GetAppName
{
    // get application name
    return [[[NSBundle mainBundle]infoDictionary]objectForKey:@"CFBundleName"];
}
//------------------------------------------------------------------------------
+ (NSString*) GetAppVersionStr
{
    // get application version
    return [[[NSBundle mainBundle]infoDictionary]objectForKey:@"CFBundleVersion"];
}
//------------------------------------------------------------------------------
+ (NSString*) GetOSName
{
    NSProcessInfo* pSystemInfo = [NSProcessInfo processInfo];
    return [pSystemInfo operatingSystemName];
}
//------------------------------------------------------------------------------
+ (NSString*) GetOSVersionStr
{
    NSProcessInfo* pSystemInfo = [NSProcessInfo processInfo];
    return [pSystemInfo operatingSystemVersionString];
}
//------------------------------------------------------------------------------
+ (NSString*) GetMemoryStr
{
    // get target machine physical memory
    NSProcessInfo* pSystemInfo        = [NSProcessInfo processInfo];
    unsigned long long physicalMemory = [pSystemInfo physicalMemory];

    // try to convert memory in GB
    std::size_t memory = physicalMemory / (1024 * 1024 * 1024);

    // conversion succeeded?
    if (memory >= 1)
        // return memory in GB
        return [[[NSString alloc]initWithFormat:@"%lu GB", memory]autorelease];

    // try to convert memory in MB
    memory = physicalMemory / (1024 * 1024);

    // conversion succeeded?
    if (memory >= 1)
        // return memory in MB
        return [[[NSString alloc]initWithFormat:@"%lu MB", memory]autorelease];

    // try to convert memory in KB
    memory = physicalMemory / 1024;

    // conversion succeeded?
    if (memory >= 1)
        // return memory in KB
        return [[[NSString alloc]initWithFormat:@"%lu KB", memory]autorelease];

    // return memory in bytes
    return [[[NSString alloc]initWithFormat:@"%llu B", physicalMemory]autorelease];
}
//------------------------------------------------------------------------------
+ (BOOL) FileExists :(NSString*)pFileName
{
    NSFileManager* pFileManager= [NSFileManager defaultManager];

    BOOL isDir = NO;

    // dir or file exists at location?
    if (![pFileManager fileExistsAtPath:pFileName isDirectory:&isDir])
        return NO;

    // is really a file?
    return !isDir;
}
//------------------------------------------------------------------------------
+ (BOOL) DirExists :(NSString*)pDirName
{
    NSFileManager* pFileManager= [NSFileManager defaultManager];
    
    BOOL isDir = NO;
    
    // dir or file exists at location?
    if (![pFileManager fileExistsAtPath:pDirName isDirectory:&isDir])
        return NO;
    
    // is really a directory?
    return isDir;
}
//------------------------------------------------------------------------------
+ (BOOL) CreateDir :(NSString*)pDirName :(BOOL)createIntermediate
{
    // directory already exists?
    if ([self DirExists:pDirName])
        return YES;

    NSFileManager* pFileManager= [NSFileManager defaultManager];
    NSError*       pError;

    // try to create directory
    if (![pFileManager createDirectoryAtPath:pDirName
                       withIntermediateDirectories:createIntermediate
                       attributes:nil
                       error:&pError])
    {
        #ifdef M_USE_LOGGER
            M_LogErrorT("Create dir - FAILED - "
                        << [X_StringTools NSStrToWStr:[pError localizedFailureReason]]
                        << " - "
                        << [X_StringTools NSStrToWStr:pDirName]);
        #endif

        return NO;
    }

    return YES;
}
//------------------------------------------------------------------------------
+ (unsigned long) CountFiles :(NSString*)pDirName
{
    if (![self DirExists:pDirName])
        return 0;

    // get directory content
    NSError* pError;
    NSArray* pDirectoryContent = [[NSFileManager defaultManager]
                                  contentsOfDirectoryAtPath:pDirName
                                  error:&pError];

    // failed to get dir content?
    if (!pDirectoryContent)
    {
        #ifdef M_USE_LOGGER
            M_LogErrorT("Count files - FAILED - "
                        << [X_StringTools NSStrToWStr:[pError localizedFailureReason]]
                        << " - "
                        << [X_StringTools NSStrToWStr:pDirName]);
        #endif

        return 0;
    }

    // returns number of files contained in directory
    return [pDirectoryContent count];
}
//------------------------------------------------------------------------------
+ (NSString*) GetiOSDocumentDir
{
    // get iOS document directory
    NSArray* pPaths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, 
                                                          NSUserDomainMask,
                                                          YES);

    return [pPaths objectAtIndex:0];
}
//------------------------------------------------------------------------------
+ (NSString*) GetiOSResourcesDir
{
    return [[NSBundle mainBundle]resourcePath];
}
//------------------------------------------------------------------------------
+ (BOOL) DeleteFile :(NSString*)pFileName
{
    if (![self FileExists:pFileName])
        return YES;

    NSFileManager* pFileManager= [NSFileManager defaultManager];
    NSError*       pError;

    // is file accessible?
    if ([pFileManager isReadableFileAtPath:pFileName])
        // try to delete file
        if (![pFileManager removeItemAtPath:pFileName error:&pError])
        {
            #ifdef M_USE_LOGGER
                M_LogErrorT("Delete file - FAILED - "
                            << [X_StringTools NSStrToWStr:[pError localizedFailureReason]]
                            << " - "
                            << [X_StringTools NSStrToWStr:pFileName]);
            #endif

            return NO;
        }

    return YES;
}
//------------------------------------------------------------------------------
+ (NSString*) GetIPAddress
{
    NSString*       pAddress    = @"error";
    struct ifaddrs* pInterfaces = NULL;
    struct ifaddrs* pTemp_addr  = NULL;
    int             success     = 0;

    // get current interfaces, returns 0 on success
    success = getifaddrs(&pInterfaces);

    if (success != 0)
        return pAddress;

    // loop through linked list of interfaces
    pTemp_addr = pInterfaces;

    while (pTemp_addr != NULL)
    {
        if (pTemp_addr->ifa_addr->sa_family == AF_INET)
        {
            // check if interface is en0, which is the wifi connection on the iPhone
            if ([[NSString stringWithUTF8String:pTemp_addr->ifa_name] isEqualToString:@"en0"])
                // get NSString from C String
                pAddress = [NSString stringWithUTF8String:
                              inet_ntoa(((struct sockaddr_in *)pTemp_addr->ifa_addr)->sin_addr)];
        }

        pTemp_addr = pTemp_addr->ifa_next;
    }

    // free memory
    freeifaddrs(pInterfaces);
    return pAddress;
}
//------------------------------------------------------------------------------
+ (NSString*) GetPlatform
{
    size_t size;
    sysctlbyname("hw.machine", NULL, &size, NULL, 0);

    char* pMachine = new char[size];
    sysctlbyname("hw.machine", pMachine, &size, NULL, 0);

    NSString* pPlatform = [NSString stringWithCString:pMachine encoding:NSUTF8StringEncoding];

    delete[] pMachine;
    return pPlatform;
}
//------------------------------------------------------------------------------
+ (NSString*) GenerateUUIDString
{
    // create new UUID
    CFUUIDRef uuid = CFUUIDCreate(kCFAllocatorDefault);

    // get UUID string
    NSString *uuidString = (NSString *)CFUUIDCreateString(kCFAllocatorDefault, uuid);

    // set new UUID string autorelease
    [uuidString autorelease];

    // release UUID
    CFRelease(uuid);

    return uuidString;
}
//------------------------------------------------------------------------------
@end
//------------------------------------------------------------------------------
