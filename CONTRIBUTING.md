# Contributing to MMOSMacs

Howdy, and welcome to the MMOSMacs git repository. This document
outlines the conventions and standards for the project.


## This is personal

Please understand that this is my personal Emacs configuration. MMOSMacs
is designed to meet my personal needs. I welcome any and all
contributions, and I will do my best to make MMOSMacs accessible to
others, but my needs and preferences ultimately take priority.

All contributors are listed at the end of this document. If you have
contributed and your name does not appear here, submit a PR or RFC Issue
for it to be added.


## Code of Conduct

I will reject all contributions from:
- people who abuse other contributors.
- people I know to be affiliated with any state agency.

Furthermore, individual items will be rejected because of:
- exceptionally vulgar language.
- ignorance or disregard of the `STYLE_GUIDE.md` or Issue templates.


## How to Contribute

There are several ways you can contribute.

The easiest way you can contribute is by taking part in the discussions
on the [GitHub Issues page](https://github.com/MountainMan1312/MMOSMacs/issues).
You can comment on topics that already have an existing Issue, or you
can submit your own if it does not already exist. Issues can take the
form of:
- **RFC**: discussion about making a change to MMOSMacs.
- **Bug Report**: discusion about a problem with the MMOSMacs code.
- **Feedback**: share your thoughts about MMOSMacs.

Before submitting a new issue, search for existing duplicate issues. If
it already exists, comment on the existing Issue instead of making a new
one. Duplicate issues will be removed and you will be asked to move your
post to the comment section of the existing post. It's not to be rude,
it's just for organization's sake.

To contribute code:
1. Make an RFC or find an existing one
2. Discuss your proposed changes
3. Create a branch
4. Make your changes
5. Submit a Pull Request


### RFC Issues, branches, commits, & Pull Requests

#### Writing an RFC

RFCs are the main form of project management for this repository. All
major changes to MMOSMacs should have an associated RFC. RFC Issues
should be named in the format `RFC: TITLE`, where title is a short
description of the proposed changes. For example, the RFC to modify a
few default Emacs things as a starting point for MMOSMacs is called
`RFC: Start with sensible defaults`.

The following template should be used when writing an RFC.

```markdown
# Summary

Provide a one-paragraph summary of the proposed changes


# Rationale and motivation

Explain why this change is necessary or desirable. Relate to any other
relevant RFCs, Bug Reports, or Feedback. Feel free to link to Issues
from other repositories as well.


# Details

Provide an in-depth description of your proposed changes. This should
get into specifics and corner-cases.


# Drawbacks and Alternatives (optional)

Why should these changes NOT be implemented? What other options are
there, and why are these changes better than the alternatives?


# Additional resources (optional)

Link to external resources such as research papers, images, etc.


# Unresolved questions (optional)

Ask any further questions you have.
```


#### Creating branches & committing changes

All changes made as part of an RFC should be made in an associated
branch. The branch should be named in the format `TYPE/#/TITLE`, where
TYPE is one of `RFC, BUG`, X is the Issue number, and TITLE is a short
summary of the RFC title. For example the branch for
`RFC: Start with sensible defaults` is named `RFC/2/sensible-defaults`.
Branch names should be short, with an absolute maximum of 72 characters
(preferrably shorter).

Commits should be small and nuclear. They should each do "one thing".
That said, multiple files could theoretically be affected by a single
commit. Commit messages should be brief, but not omit any necessary
detail.


#### Writing a Pull Request

Like RFCs, all major changes to MMOSMacs must have an associated Pull
Reqest, which explains in detail the changes that were made. Pull
Requests are reviewed before the changes are accepted into the stable
branch, and it is likely that the reviewer will reject your changes with
notes on what needs to be fixed.

Pull Requests should explain the specifics and nuances of the changes
contained therein.
