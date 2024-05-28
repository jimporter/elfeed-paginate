# elfeed-paginate

**elfeed-paginate** adds the ability to retrieve multiple pages of results for a
web feed. Currently, it only supports pagination of WordPress feeds.

## Initial setup

To enable pagination for feeds, simply call `elfeed-paginate`. If you use
`use-package`, you can configure it like so:

```elisp
(use-package elfeed-paginate
  :after (elfeed)
  :config (elfeed-paginate))
```

## Customization

To add support for a new style of pagination, you can add a new entry to
`elfeed-paginate-next-page-url-hook`. Each entry should take the URL that was
just fetched, its XML, and the Elfeed feed object. Elfeed will use the first
non-nil result. If the result is a symbol, this means that there is no next
page.

## License

This code is licensed under the terms of the GNU General Public License, version
3 or later.
