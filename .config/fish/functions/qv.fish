function qv
    # Thanks https://github.com/davidgasquez/dotfiles/blob/bb9df4a369dbaef95ca0c35642de491c7dd41269/shell/zshrc#L50-L99
    # and
    # https://simonwillison.net/2024/Dec/19/q-and-qv-zsh-functions/

    # Arguments
    set url $argv[1]
    set question $argv[2]

    # Fetch the URL content through Jina
    # Use yt-dlp to get the subtitle URL
    # set subtitle_url (yt-dlp -q --skip-download --convert-subs srt --write-sub --sub-langs "en" --write-auto-sub --print "requested_subtitles.en.url" "$url")
    set subtitle_url (yt-dlp -q --skip-download --convert-subs srt --write-sub --sub-langs "en" --write-auto-sub --print "requested_subtitles.en.url,title" "$url")

    if test (count $subtitle_url) -eq 2
        set video_title $subtitle_url[2]
        set subtitle_url $subtitle_url[1]
    else
        echo "Error: Expected two lines of output, but got (count $subtitle_url)." >&2
        return 1
    end

    # Fetch subtitles and clean content
    set content (curl -s "$subtitle_url" \
        | sed '/^$/d' \
        | grep -v '^[0-9]*$' \
        | grep -v '\-->' \
        | sed 's/<[^>]*>//g' \
        | tr '\n' ' ')

    # Check if the content was retrieved successfully
    if test -z "$content"
        echo "Failed to retrieve content from the URL."
        return 1
    end

    # Define the system prompt
    set system "
You are a helpful assistant that can answer questions about YouTube videos.
Reply concisely, in a few sentences or dot points.

Include the title of the video which is:
$video_title

The content:
$content
"

    # Use llm with the fetched content as a system prompt
    llm prompt "$question" -s "$system"
end
