#import <Foundation/Foundation.h>
 
@interface Person : NSObject

{
    NSString *personName;
    NSInteger personAge;
}

- (id)initWithName:(NSString *)name andAge:(NSInteger)age;
- (void)print;
@end

@implementation Person

- (id)initWithName:(NSString *)name andAge:(NSInteger)age{
    personName = name;
    personAge = age;
    return self;
}

- (void)print{
    NSLog(@"Name: %@", personName);
    NSLog(@"Age: %ld", personAge);
}

@end

@interface Employee : Person

{
    NSString *employeeEducation;
}

- (id)initWithName:(NSString *)name andAge:(NSInteger)age 
  andEducation:(NSString *)education;
- (void)print;

@end


@implementation Employee

- (id)initWithName:(NSString *)name andAge:(NSInteger)age 
  andEducation: (NSString *)education
  {
    personName = name;
    personAge = age;
    employeeEducation = education;
    return self;
}

- (void)print
{
    [super print];
    NSLog(@"Education: %@", employeeEducation);
}

@end


int main(int argc, const char * argv[])
{
    NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];        
    NSLog(@"Base class Person Object");
    Person *person = [[Person alloc]initWithName:@"Raj" andAge:5];
    [person print];
    NSLog(@"Inherited Class Employee Object");
    Employee *employee = [[Employee alloc]initWithName:@"Raj" 
    andAge:5 andEducation:@"MBA"];
    [employee print];        
    [pool drain];
    return 0;
}