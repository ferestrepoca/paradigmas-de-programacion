#import <Foundation/Foundation.h>

@interface Car:NSObject{
	NSString * brand;
}

@property(nonatomic,readwrite,copy) NSString * brand;

-(void)drive;

@end