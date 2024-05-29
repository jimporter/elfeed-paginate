# elfeed-paginate

**elfeed-paginate** adds the ability to retrieve multiple pages of results for a
web feed. Currently, it supports pagination using [RFC 5005][rfc-5005] links and
for WordPress feeds.

## Configuration

To enable pagination for feeds, simply call `elfeed-paginate`. If you use
`use-package`, you can configure it like so:

```elisp
(use-package elfeed-paginate
  :after (elfeed)
  :config (elfeed-paginate))
```

You can customize the number of pages to retrieve for feeds by setting
`elfeed-paginate-max-pages` (the default is 5).

### Supporting more feeds

To add support for a new style of pagination, add a new function to
`elfeed-paginate-next-page-url-hook`. This function should take the URL that was
just fetched, its XML, and the Elfeed feed object. Elfeed will use the first
non-nil result from this hook. If that result is a symbol, this means that there
is no next page.

## Backfilling

For existing feeds, you can backfill several pages of results using
`elfeed-paginate-backfill`. By default, this will fetch at most
`elfeed-paginate-max-pages`; with a prefix argument, fetch at most that many
pages instead.

## License

This code is licensed under the terms of the GNU General Public License, version
3 or later.

[rfc-5005]: https://datatracker.ietf.org/doc/html/rfc5005
