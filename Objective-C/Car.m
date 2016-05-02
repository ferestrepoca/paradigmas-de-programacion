# import "Car.h"

@implementation Car

@synthesize brand;

-(id)init{
	self = [super init];
	brand = @"";
	return self;
}

-(void)drive{
	NSLog(@"I'm driving a %@",brand);
}

@end