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

-- Extract year from email date header
-- @param message: single message to extract year from
-- @return year as string (e.g., "2025")
function get_message_year(message)
	-- Get the message headers including Date
	local mbox, uid = table.unpack(message)
	local date_header = mbox[uid]:fetch_header()

	-- Try to extract year from Date header
	-- Date format is usually like: "Thu, 10 Dec 2025 10:30:45 +0100"
	local year = date_header:match("Date:.-(%d%d%d%d)")

	-- Fallback to current year if we can't parse the date
	if not year then
		year = os.date("%Y")
	end

	return year
end

-- Archive messages by year
-- @param message_set: set of messages to archive
function archive_by_year(message_set)
	-- Group messages by year
	local messages_by_year = {}

	for _, msg in ipairs(message_set) do
		local year = get_message_year(msg)
		if not messages_by_year[year] then
			messages_by_year[year] = Set {}
		end
		messages_by_year[year] = messages_by_year[year] + Set {msg}
	end

	-- Move messages to year-based archive folders
	for year, msgs in pairs(messages_by_year) do
		local folder_name = 'Archive/' .. year
		-- Ensure the folder exists
		account:create_mailbox(folder_name)
		msgs:move_messages(account[folder_name])
	end
end

-- Filter messages from a list of senders
-- @param messages: message set to filter
-- @param senders: table/list of email addresses
-- @param action: 'archive', 'move', or 'delete'
-- @param folder_name: folder name for moving (only used for 'move' action)
function filter_by_senders(messages, senders, action, folder_name)
	local results = Set {}

	for _, sender in ipairs(senders) do
		results = results + messages:contain_from(sender)
	end

	if action == 'delete' then
		results:delete_messages()
	elseif action == 'archive' then
		archive_by_year(results)
	elseif action == 'move' then
		if not folder_name then
			print("Error: folder_name required for 'move' action")
			return results
		end
		results:move_messages(account[folder_name])
	else
		print("Unknown action: " .. action .. ". Use 'archive', 'move', or 'delete'")
	end

	return results
end

----------
-- Email Category Filters
----------

local todelete = {
	'contact@news.probikeshop.com',
	'adidas@fr-news.adidas.com',
	'contact@email.westfield.com',
	'soouest@email.westfield.com',
	'geox@email-geox.com',
	'shop@emails.flic.io',
	'shop@mail.nova.fr',
	'noreply-marketplace.partner@decathlon.com',
	'noreply@e-ticket.jacadi.com',
	'bonjour@ferflex.fr',
	'news@email.arthur.fr',
	'mailgun@mg.welmo.fr',
	'info@kiddiprint.com',
	'confirmation-commande@amazon.fr',
	'shipment-tracking@amazon.fr',
	'order-update@amazon.fr',
	'noreply@audible.fr',
	'team@email.remarkable.com',
	'contact@thepihut.com',
	'email.campaign@sg.booking.com',
	'info@e.sixt.fr',
	'noreply@komoot.de',
}

local toarchive = {
	'pragmaticengineer@substack.com',
	'pragmaticengineer+deepdives@substack.com',
	'newsletter@farnamstreetblog.com',
	'james@jamesclear.com',
	'bloodinthemachine@substack.com',
	'peter@golangweekly.com',
	'learn@semaphore.io',
	'todoist@substack.com',
	'hello@readwise.io',
	'newsletter@quotidien.fr',
	'support@pragprog.com',
	'noreply@7digital.com',
}
filter_by_senders(messages, todelete, 'delete')
filter_by_senders(messages, toarchive, 'archive')
