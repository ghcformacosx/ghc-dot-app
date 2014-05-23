//
//  GHCAppDelegate.h
//  GHC
//
//  Created by Bob Ippolito on 5/22/14.
//  Copyright (c) 2014 GHC for Mac OS X. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface GHCAppDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSImageView *haskellIcon;
@property (assign) IBOutlet NSTextField *shellCopy;
- (IBAction)openDocs:(id)sender;
@end
