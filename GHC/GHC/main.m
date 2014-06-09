//
//  main.m
//  GHC
//
//  Created by Bob Ippolito on 5/22/14.
//  Copyright (c) 2014 GHC for Mac OS X. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "GHCAppDelegate.h"

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        NSApplication *app = [NSApplication sharedApplication];
        [app setActivationPolicy:NSApplicationActivationPolicyRegular];
        GHCAppDelegate *delegate = [GHCAppDelegate new];
        app.delegate = delegate;
        [app activateIgnoringOtherApps:YES];
        [app run];
    }
    return EXIT_SUCCESS;
}
