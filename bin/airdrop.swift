import AppKit
import Foundation

let args = CommandLine.arguments
guard args.count > 1 else {
    fputs("Usage: airdrop <file>\n", stderr)
    exit(1)
}

let url = URL(fileURLWithPath: args[1])

guard FileManager.default.fileExists(atPath: url.path) else {
    fputs("File not found: \(args[1])\n", stderr)
    exit(1)
}

guard let service = NSSharingService(named: .sendViaAirDrop) else {
    fputs("AirDrop not available\n", stderr)
    exit(1)
}

NSApplication.shared.setActivationPolicy(.accessory)

class Delegate: NSObject, NSSharingServiceDelegate {
    func sharingService(_ service: NSSharingService, didShareItems items: [Any]) {
        exit(0)
    }
    func sharingService(_ service: NSSharingService, didFailToShareItems items: [Any], error: Error) {
        let cancelled = (error as NSError).code == NSUserCancelledError
        exit(cancelled ? 2 : 1)
    }
}

let delegate = Delegate()
service.delegate = delegate
service.perform(withItems: [url])

NSApplication.shared.run()
