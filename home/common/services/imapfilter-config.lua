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
github = messages:contain_from('notifications@github.com')
github:move_messages(account['GitHub'])

-- Example 2: Move mailing list emails
-- Uncomment and customize as needed
-- lists = messages:contain_to('list@example.com')
-- lists:move_messages(account['Lists'])

-- Example 3: Filter by subject
-- Uncomment and customize as needed
-- spam = messages:contain_subject('[SPAM]')
-- spam:move_messages(account['Junk'])

----------
-- Helper Functions
----------

-- Filter messages from a list of senders
-- @param messages: message set to filter
-- @param senders: table/list of email addresses
-- @param action: 'archive' or 'delete'
-- @param archive_folder: folder name for archiving (optional, defaults to 'Archives')
function filter_by_senders(messages, senders, action, archive_folder)
  archive_folder = archive_folder or 'Archives'
  local results = Set {}

  for _, sender in ipairs(senders) do
    results = results + messages:contain_from(sender)
  end

  if action == 'delete' then
    results:delete_messages()
  elseif action == 'archive' then
    results:move_messages(account[archive_folder])
  else
    print("Unknown action: " .. action .. ". Use 'archive' or 'delete'")
  end

  return results
end

----------
-- Marketing Email Filters
----------

-- List of marketing senders to archive
local to_archive = {
  -- Add your marketing email addresses here
  -- 'newsletter@example.com',
  -- 'marketing@company.com',
}

-- List of marketing senders to delete
local to_delete = {
  -- Add email addresses to delete here
  -- 'spam@example.com',
}

-- Apply filters (uncomment to enable)
-- filter_by_senders(messages, to_archive, 'archive')
-- filter_by_senders(messages, to_delete, 'delete')
