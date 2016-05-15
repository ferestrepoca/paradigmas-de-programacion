# import "Car.h"

int main(){
	
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	Car * car1 = [[Car alloc] init];
	car1.brand = @"Mercedez Benz";

	Car * car2 = [[Car alloc] init];
	car2.brand = @"Mazada";

	Car * car3 = [[Car alloc] init];
	car3.brand = @"Chevrolet";

	[car1 drive];
	[car2 drive];
	[car3 drive];

	[pool drain];
	return 0;
}