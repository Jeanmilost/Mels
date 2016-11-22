/*****************************************************************************
 * ==> QRX_StringTools ------------------------------------------------------*
 * ***************************************************************************
 * Description : Some tools to convert std::string <==> NSString             *
 * Developper  : Jean-Milost Reymond                                         *
 *****************************************************************************/

#import <Foundation/Foundation.h>
#include <string>

/**
* Tool class to facilitate usage of strings and NSStrings
*@note This class is Apple xCode specific
*@author Jean-Milost Reymond
*/
@interface QRX_StringTools : NSObject
{}

/**
* Converts an NSString to std::string
*@param pStr - string to convert
*@returns converted string
*/
+ (std::string)NSStrToStr :(NSString*)pStr;

/**
* Converts an NSString to std::wstring
*@param pStr - string to convert
*@returns converted string
*/
+ (std::wstring)NSStrToWStr :(NSString*)pStr;

/**
* Converts an std::string to NSString
*@param str - string to convert
*@returns converted string
*/
+ (NSString*)StrToNSStr :(const std::string&)str;

/**
* Converts an std::wstring to NSString
*@param wStr - string to convert
*@returns converted string
*/
+ (NSString*)WStrToNSStr :(const std::wstring&)wStr;

@end
