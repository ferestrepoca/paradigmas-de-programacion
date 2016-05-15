# import "Box.h"

int main( )
{
   NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];    
   Box *box1 = [[Box alloc]init];    // Create box1 object of type Box
   Box *box2 = [[Box alloc]init];    // Create box2 object of type Box
   Box *box3 = [[Box alloc]init];    // Create box3 object of type Box

   double volume = 0.0;     // Store the volume of a box here
 
   // box 1 specification
   box1.height = 5.0; 

   // box 2 specification
   box2.height = 10.0;
  
   // box 3 specification
   box3.height = 111.0;

   // volume of box 1
   volume = [box1 volume];
   NSLog(@"Volume of Box1 : %f", volume);
   // volume of box 2
   volume = [box2 volume];
   NSLog(@"Volume of Box2 : %f", volume);
  
   volume = [box3 volume];
   NSLog(@"Volume of Box3 : %f", volume);

   [pool drain];
   return 0;
}