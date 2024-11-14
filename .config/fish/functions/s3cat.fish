function s3cat --argument url
	set filename (basename $url)
	set extension (string split "." $filename)[-1]
	s5cmd cat $url | bat --language $extension --file-name $url -

end
