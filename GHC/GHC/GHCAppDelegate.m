//
//  GHCAppDelegate.m
//  GHC
//
//  Created by Bob Ippolito on 5/22/14.
//  Copyright (c) 2014 GHC for Mac OS X. All rights reserved.
//

#import "GHCAppDelegate.h"
#import <JavaScriptCore/JavaScriptCore.h>
#import <OpenDirectory/OpenDirectory.h>

static NSString *getUserShell() {
    // We can't trust the environment to reflect the current shell, so get
    // it from OpenDirectory.
    ODSession *session = [ODSession defaultSession];
    ODNode *node;
    ODQuery *query;
    NSError *err;
    NSString *defaultShell = [NSProcessInfo processInfo].environment[@"SHELL"];
    node = [ODNode nodeWithSession:session type:kODNodeTypeLocalNodes error:&err];
    if (!node) {
        NSLog(@"err getting node: %@", err);
        return defaultShell;
    }
    query = [ODQuery queryWithNode:node
                    forRecordTypes:kODRecordTypeUsers
                         attribute:kODAttributeTypeRecordName
                         matchType:kODMatchEqualTo
                       queryValues:NSUserName()
                  returnAttributes:kODAttributeTypeStandardOnly
                    maximumResults:1
                             error:&err];
    if (!query) {
        NSLog(@"err getting query: %@", err);
        return defaultShell;
    }
    NSArray *results = [query resultsAllowingPartial:NO error:&err];
    if (!results) {
        NSLog(@"err getting results: %@", err);
        return defaultShell;
    }
    for (ODRecord *result in results) {
        NSArray *vals = [result valuesForAttribute:kODAttributeTypeUserShell error:&err];
        if (!vals) {
            NSLog(@"err getting UserShell: %@", err);
        } else if (vals.count > 0) {
            return [vals firstObject];
        }
    }
    return defaultShell;
}

static NSDictionary *parseShellOutput(NSString *launchPath) {
    NSMutableDictionary *dict = [NSMutableDictionary new];
    NSPipe *pipe = [NSPipe pipe];
    NSFileHandle *file = pipe.fileHandleForReading;
    NSTask *task = [NSTask new];
    task.launchPath = getUserShell();
    task.arguments = @[launchPath];
    task.standardOutput = pipe;
    [task launch];
    NSData *data = [file readDataToEndOfFile];
    [file closeFile];
    NSString *output = [[NSString alloc] initWithData: data encoding: NSUTF8StringEncoding];
    NSArray *lines = [output componentsSeparatedByCharactersInSet:[NSCharacterSet newlineCharacterSet]];
    for (NSString *line in lines) {
        NSString *key;
        NSScanner *scanner = [NSScanner scannerWithString:line];
        if (![scanner scanUpToString:@": " intoString:&key]) continue;
        if (![scanner scanString:@": " intoString:nil]) continue;
        NSString *value = [line substringFromIndex:scanner.scanLocation];
        dict[key] = value;
    }
    return dict;
}

static NSMenu *mkMenu(NSString *title, NSArray *items) {
    NSMenu *menu = [NSMenu new];
    menu.title = title;
    for (NSMenuItem *item in items) {
        [menu addItem:item];
    }
    return menu;
}

static NSMenuItem *mkItem(NSString *title, SEL action, NSString *keyEquivalent) {
    return [[NSMenuItem alloc] initWithTitle:title action:action keyEquivalent:keyEquivalent];
}

static NSMenu *mkMainMenu(NSArray *subMenus) {
    NSMenu *mainMenu = [NSMenu new];
    for (NSMenu *subMenu in subMenus) {
        [mainMenu setSubmenu:subMenu
                     forItem:[mainMenu addItemWithTitle:subMenu.title
                                                 action:NULL
                                          keyEquivalent:@""]];
    }
    return mainMenu;
}

@interface GHCAppDelegate()

@property NSWindow *window;
@property WebView *webView;
@property NSData *bundleBookmark;
@end

@implementation GHCAppDelegate

- (instancetype)init
{
    if (self = [super init]) {
        /* initialize WebKit debugging */
        [[NSUserDefaults standardUserDefaults] setBool:TRUE forKey:@"WebKitDeveloperExtras"];
        [[NSUserDefaults standardUserDefaults] synchronize];

        /* get some metadata about the app */
        NSBundle *bundle = [NSBundle mainBundle];
        NSString *appName = bundle.infoDictionary[@"CFBundleName"];
        NSString *version = bundle.infoDictionary[@"CFBundleShortVersionString"];

        /* bookmark our bundle URL */
        self.bundleBookmark = [[bundle bundleURL] bookmarkDataWithOptions:0
                                           includingResourceValuesForKeys:@[]
                                                            relativeToURL:nil
                                                                    error:NULL];

        /* Create the menus */
        NSMenu *mainMenu = mkMainMenu(@[
            mkMenu(appName,
                   @[mkItem([@"Quit " stringByAppendingString:appName], @selector(terminate:), @"q")]),
            mkMenu(@"File",
                   @[mkItem(@"Relaunch", @selector(relaunch:), @"r"),
                     [NSMenuItem separatorItem],
                     mkItem(@"Close", @selector(terminate:), @"w")]),
            mkMenu(@"Edit",
                   @[mkItem(@"Copy", @selector(copy:), @"c")]),
            mkMenu(@"Window",
                   @[mkItem(@"Minimize", @selector(performMiniaturize:), @"m"),
                     mkItem(@"Zoom", @selector(performZoom:), @""),
                     [NSMenuItem separatorItem],
                     mkItem(@"Bring all to front", @selector(arrangeInFront:), @"")]),
            mkMenu(@"Help",
                   @[mkItem(@"GHC Documentation", @selector(openDocs:), @""),
                     mkItem(@"Haskell for Mac OS X Homepage", @selector(openHomepage:), @""),
                     mkItem(@"Report an Issue", @selector(openIssue:), @"")])

        ]);

        [NSApp setMainMenu:mainMenu];

        /* Set up the window and webView */
        NSWindow *window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 700, 400)
                                                  styleMask:NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask
                                                    backing:NSBackingStoreBuffered
                                                      defer:NO];
        window.title = [NSString stringWithFormat:@"%@ %@", appName, version];
        window.delegate = self;
        self.webView = [[WebView alloc] initWithFrame:window.frame
                                       frameName:nil
                                       groupName:nil];
        self.webView.frameLoadDelegate = self;
        self.webView.UIDelegate = self;
        self.webView.policyDelegate = self;
        window.contentView = self.webView;
        [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
        self.window = window;
        window.frameAutosaveName = @"MainWindow";
        [self refresh:self];
    }
    return self;
}

- (NSURL *)appURL
{
    return [NSURL URLByResolvingBookmarkData:self.bundleBookmark
                                     options:NSURLBookmarkResolutionWithoutUI
                               relativeToURL:nil
                         bookmarkDataIsStale:NULL
                                       error:NULL];
}

- (NSString *)bundlePath
{
    NSURL *appURL = [self appURL];
    return appURL ? appURL.path : [NSBundle mainBundle].bundlePath;
}

- (NSDictionary *)ghcPaths
{
    return parseShellOutput([[NSBundle mainBundle] pathForResource:@"read_environment" ofType:@"sh"]);
}

- (void)webView:(WebView *)webView decidePolicyForNavigationAction:(NSDictionary *)actionInformation request:(NSURLRequest *)request frame:(WebFrame *)frame decisionListener:(id<WebPolicyDecisionListener>)listener
{
    NSURL *url = request.URL;
    if ([url.scheme isEqualToString:@"ghcdotapp"]) {
        if ([url.host isEqualToString:@"show-in-finder"]) {
            [[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:@[[NSBundle mainBundle].bundleURL]];
        } else if ([url.host isEqualToString:@"install-xcode"]) {
            [[NSTask launchedTaskWithLaunchPath:@"/usr/bin/xcode-select" arguments:@[@"--install"]] waitUntilExit];
        } else if ([url.host isEqualToString:@"relaunch"]) {
            [self relaunch:webView];
        } else if ([url.host isEqualToString:@"refresh"]) {
            [self refresh:webView];
        } else if ([url.host isEqualToString:@"open-docs"]) {
            [self openDocs:webView];
        } else if ([url.host isEqualToString:@"man"]) {
            [self openMan:url.lastPathComponent sender:webView];
        } else if ([url.host isEqualToString:@"append-profile"]) {
            [self appendProfile:url.lastPathComponent sender:webView];
        }
        [listener ignore];
    } else {
        [listener use];
    }
}

- (NSArray *)webView:(WebView *)sender contextMenuItemsForElement:(NSDictionary *)element defaultMenuItems:(NSArray *)defaultMenuItems
{
    return defaultMenuItems;
}

- (void)webView:(WebView *)sender didClearWindowObject:(WebScriptObject *)windowObject forFrame:(WebFrame *)frame
{
    NSBundle *bundle = [NSBundle mainBundle];
    BOOL inDownloads = NO;
    NSFileManager *fileManager = [NSFileManager defaultManager];
    for (NSURL *downloadsURL in [fileManager URLsForDirectory:NSDownloadsDirectory inDomains:NSUserDomainMask]) {
        inDownloads = inDownloads || [bundle.bundleURL.URLByDeletingLastPathComponent isEqual:downloadsURL];
    }
    NSDictionary *infoDictionary = bundle.infoDictionary;
    NSDictionary *env = [NSProcessInfo processInfo].environment;
    NSMutableDictionary *profiles = [NSMutableDictionary new];
    NSString *homeDir = NSHomeDirectory();
    for (NSString *profilePath in @[@".bashrc", @".bash_profile", @".bash_login", @".profile", @".zshenv"]) {
        profiles[[profilePath substringFromIndex:1]] = @([fileManager fileExistsAtPath:[homeDir stringByAppendingPathComponent:profilePath]]);
    };
    NSString *bundlePath = [self bundlePath];
    NSDictionary *info = @{@"bundleName"    : infoDictionary[@"CFBundleName"],
                           @"bundleVersion" : infoDictionary[@"CFBundleShortVersionString"],
                           @"appName"       : bundlePath.lastPathComponent.stringByDeletingPathExtension,
                           @"bundlePath"    : bundlePath,
                           @"environment"   : env,
                           @"paths"         : self.ghcPaths,
                           @"profiles"      : profiles,
                           @"inDownloadsDir": @(inDownloads)};
    // NSDictionary isn't bridged as you would expect, so this happened.
    NSData *jsonData = [NSJSONSerialization dataWithJSONObject:info options:0 error:NULL];
    NSString *jsonString = [[NSString alloc] initWithData:jsonData encoding:NSUTF8StringEncoding];
    JSContextRef ctx = sender.mainFrame.globalContext;
    JSValueRef jsInfo = JSValueMakeFromJSONString(ctx, JSStringCreateWithCFString((__bridge CFStringRef)jsonString));
    JSObjectSetProperty(ctx,
                        [windowObject JSObject],
                        JSStringCreateWithCFString((__bridge CFStringRef)@"info"),
                        jsInfo,
                        kJSPropertyAttributeNone,
                        NULL);
}

- (void)applicationWillFinishLaunching:(NSNotification *)notification
{
    [self.window makeKeyAndOrderFront:self];
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)app
{
    return YES;
}

- (void)openDocs:(id)sender
{
    NSString *html = [NSString pathWithComponents:@[[self bundlePath],
                                                    @"Contents", @"share", @"doc",
                                                    @"ghc", @"html", @"index.html"]];
    [[NSWorkspace sharedWorkspace] openFile:html];
}

- (void)openHomepage:(id)sender
{
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"https://ghcformacosx.github.io/"]];
}

- (void)openIssue:(id)sender
{
    [[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"https://github.com/ghcformacosx/ghc-dot-app/issues"]];
}

- (void)refresh:(id)sender
{
    NSURL *url = [[self appURL] URLByAppendingPathComponent:@"Contents/Resources/html/index.html"];
    [self.webView.mainFrame loadRequest:[NSURLRequest requestWithURL:url]];
}

- (void)relaunch:(id)sender
{
    [NSTask launchedTaskWithLaunchPath:@"/usr/bin/open" arguments:@[@"-n", [self bundlePath]]];
    [NSApp terminate:self];
}

- (void)openMan:(NSString *)page sender:(id)sender
{
    NSString *script = [NSString stringWithFormat:@"tell application \"Terminal\" to do script \"man 1 %@; exit\"", page];
    [[[NSAppleScript alloc] initWithSource:script] executeAndReturnError:nil];
    [[NSWorkspace sharedWorkspace] launchApplication:@"Terminal"];
}

- (void)sheetDidEndShouldAppendProfile:(NSWindow *)sheet returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo
{
}

- (void)appendProfile:(NSString *)profilePath sender:(id)sender
{
    NSString *path = [NSHomeDirectory() stringByAppendingPathComponent:[@"." stringByAppendingString:profilePath]];
    NSAlert *sheet = [NSAlert new];
    if ([[NSFileManager defaultManager] fileExistsAtPath:path]) {
        sheet.informativeText = [NSString stringWithFormat:@"Are you sure you want to append this code to your ~/.%@? A backup will be saved to ~/.%@.ghc.orig", profilePath, profilePath];
        sheet.messageText = [NSString stringWithFormat:@"Confirm .%@ modification", profilePath];
        [sheet addButtonWithTitle:@"Modify"].tag = 0;
    } else {
        sheet.informativeText = [NSString stringWithFormat:@"Are you sure you want to create a new ~/.%@?", profilePath];
        sheet.messageText = [NSString stringWithFormat:@"Confirm .%@ creation", profilePath];
        [sheet addButtonWithTitle:@"Create"].tag = 0;
    }
    [sheet addButtonWithTitle:@"Cancel"].tag = -1;
    [sheet beginSheetModalForWindow:self.window completionHandler:^(NSModalResponse returnCode) {
        if (returnCode) {
            return;
        }
        NSFileHandle *fh = [NSFileHandle fileHandleForWritingAtPath:path];
        if (!fh) {
            // Create a new file
            [[NSFileManager defaultManager] createFileAtPath:path
                                                    contents:nil
                                                  attributes:nil];
            fh = [NSFileHandle fileHandleForWritingAtPath:path];
        } else {
            // Make a backup copy
            [[NSFileManager defaultManager] copyItemAtPath:path
                                                    toPath:[path stringByAppendingString:@".ghc.orig"]
                                                     error:nil];
        }
        [fh seekToEndOfFile];
        NSString *code = [[[self.webView mainFrameDocument] querySelector:@".code-box code"] valueForKey:@"innerText"];
        [fh writeData:[[NSString stringWithFormat:@"\n%@\n", code] dataUsingEncoding:NSUTF8StringEncoding]];
        [fh closeFile];
        [self refresh:sender];
    }];
}

@end

