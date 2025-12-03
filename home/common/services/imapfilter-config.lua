----------
-- Options
----------
options.timeout = 120
options.subscribe = true

----------
-- Accounts
----------
account = IMAP {
  server = 'imap.mail.me.com',
  username = 'vdemeester@icloud.com',
  -- Password will be provided via command line or config file
  -- Using ssl by default
  ssl = 'tls1.2',
}

----------
-- Example filtering rules
-- Customize these based on your needs
----------

-- Get all messages in INBOX
messages = account['INBOX']:select_all()

-- Example 1: Move GitHub notifications to a GitHub folder
-- Uncomment and customize as needed
-- github = messages:contain_from('notifications@github.com')
-- github:move_messages(account['GitHub'])

-- Example 2: Move mailing list emails
-- Uncomment and customize as needed
-- lists = messages:contain_to('list@example.com')
-- lists:move_messages(account['Lists'])

-- Example 3: Filter by subject
-- Uncomment and customize as needed
-- spam = messages:contain_subject('[SPAM]')
-- spam:move_messages(account['Junk'])

-- Add your custom Lua rules below:
-- TODO: Add custom filtering rules here
