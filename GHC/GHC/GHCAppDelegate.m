//
//  GHCAppDelegate.m
//  GHC
//
//  Created by Bob Ippolito on 5/22/14.
//  Copyright (c) 2014 GHC for Mac OS X. All rights reserved.
//

#import "GHCAppDelegate.h"

@implementation GHCAppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    // Insert code here to initialize your application
}

- (void)awakeFromNib
{
    NSBundle *bundle = [NSBundle mainBundle];
    NSString *version = bundle.infoDictionary[@"CFBundleShortVersionString"];
    NSString *contents = [NSString pathWithComponents:@[[bundle bundlePath], @"Contents"]];
    self.window.title = [NSString stringWithFormat:@"%@ %@", self.window.title, version];
    self.haskellIcon.imageFrameStyle = NSImageFrameNone;
    self.haskellIcon.image = [NSApp applicationIconImage];
    self.shellCopy.stringValue = [NSString stringWithFormat:
                                  @"# Add this to your .bashrc\n"
                                  @"GHC_APP=\"%@\"\n"
                                  @"export PATH=\"${HOME}/.cabal/bin:${GHC_APP}/bin:${PATH}\"\n"
                                  @"", contents];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app
{
    return YES;
}

- (void)openDocs:(id)sender
{
    NSString *html = [NSString pathWithComponents:@[[[NSBundle mainBundle] bundlePath],
                                                    @"Contents", @"share", @"doc",
                                                    @"ghc", @"html", @"index.html"]];
    [[NSWorkspace sharedWorkspace] openFile:html];
}
@end
