````markdown
# Supabase-Powered Comment System (2025)

A fully client-side comment system for static websites with optional OAuth login (Google, GitHub) and anonymous posting.

---

## Tech Stack

**Frontend:**

- Static HTML pages (GitHub Pages or similar)
- 100% browser-native JavaScript (ES Modules)
- Optional CSS for styling
- `<script type="module">` import from GitHub or jsDelivr CDN

**Backend:**

- Supabase (managed serverless backend)
  - Postgres database for comments
  - Supabase Auth (anonymous + multiple OAuth providers)
  - Row Level Security (RLS) for access control
- Supabase REST API via `supabase-js` CDN import

**Maintenance:**

- Admin moderation via Supabase dashboard
- Optional GitHub Action to ping Supabase weekly (keep free tier active)

---

## Database Schema

### Comments Table

```sql
create table comments (
  id bigint generated always as identity primary key,
  page_path text not null,
  message text not null,
  name text not null,
  avatar_url text,
  user_id uuid references auth.users(id) on delete set null,
  session_id uuid,
  created_at timestamp with time zone default timezone('utc', now()),
  updated_at timestamp with time zone
);
```
````

### Admins Table (Optional)

```sql
create table admins (
  id uuid primary key references auth.users(id) on delete cascade
);
```

---

## RLS Policies

```sql
alter table comments enable row level security;

-- Anyone can read comments
create policy "public_read" on comments
for select using (true);

-- Anyone can post comments
create policy "public_insert" on comments
for insert with check (true);

-- Users can edit their own comments
create policy "owner_update" on comments
for update using (
  auth.uid() = user_id
  or session_id = auth.session_id()
)
with check (true);

-- Admins can edit any comment
create policy "admin_update" on comments
for update to authenticated using (
  exists (select 1 from admins where id = auth.uid())
);

-- Delete policies mirror update policies
create policy "owner_delete" on comments
for delete using (
  auth.uid() = user_id
  or session_id = auth.session_id()
);

create policy "admin_delete" on comments
for delete to authenticated using (
  exists (select 1 from admins where id = auth.uid())
);
```

---

## Supabase Auth Setup

1. Enable **anonymous sign-in**.
2. Enable OAuth providers:

   - Google
   - GitHub (optional)

3. Supabase allows **linking multiple OAuth providers** to the same user:

   - Anonymous → Google
   - Anonymous → GitHub
   - Google → GitHub

4. Comments remain associated with `user_id` or `session_id`.

---

## Frontend Module (`comments.js`)

**Load Supabase JS from CDN:**

```html
<script type="module">
import { createClient } from "https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm";

const SUPABASE_URL = "https://YOUR_PROJECT.supabase.co";
const SUPABASE_KEY = "YOUR_ANON_KEY";
const supabase = createClient(SUPABASE_URL, SUPABASE_KEY);
```

**Initialize session and load comments**

```js
async function initComments() {
  const container = document.getElementById("comments");
  if (!container) return;

  await ensureSession();
  await loadComments(container);

  document
    .getElementById("postComment")
    ?.addEventListener("click", postComment);
  document
    .getElementById("signInGoogle")
    ?.addEventListener("click", () => signIn("google"));
  document
    .getElementById("signInGitHub")
    ?.addEventListener("click", () => signIn("github"));
}

async function ensureSession() {
  const { data } = await supabase.auth.getSession();
  if (!data.session) await supabase.auth.signInAnonymously();
}
```

**Load comments**

```js
async function loadComments(container) {
  const { data: comments } = await supabase
    .from("comments")
    .select("*")
    .eq("page_path", window.location.pathname)
    .order("created_at", { ascending: true });

  container.innerHTML = comments.map((c) => renderComment(c)).join("");
}

function renderComment(c) {
  const edited = c.updated_at
    ? `<small>(edited ${new Date(c.updated_at).toLocaleString()})</small>`
    : "";
  return `
    <div class="comment" data-id="${c.id}">
      <strong>${c.name}</strong><br>
      <p>${c.message}</p>
      ${edited}
    </div>
  `;
}
```

**Post a comment**

```js
async function postComment() {
  const textarea = document.getElementById("commentText");
  const text = textarea.value.trim();
  if (!text) return;

  const {
    data: { user },
  } = await supabase.auth.getUser();
  const name = user?.user_metadata?.full_name || "Anonymous";
  const avatar = user?.user_metadata?.avatar_url || null;

  await supabase.from("comments").insert([
    {
      page_path: window.location.pathname,
      name,
      avatar_url: avatar,
      message: text,
      user_id: user?.id || null,
      session_id: user?.id || auth.session_id(),
    },
  ]);

  textarea.value = "";
  await loadComments(document.getElementById("comments"));
}
```

**Sign in with OAuth**

```js
async function signIn(provider) {
  const { data, error } = await supabase.auth.signInWithOAuth({ provider });
  if (error) console.error("Login failed:", error);
}
```

**Initialize**

```html
<script type="module">
  import { initComments } from "https://cdn.jsdelivr.net/gh/yourusername/comments@main/comments.js";
  initComments();
</script>
```

---

## HTML Example

```html
<div id="comments"></div>
<textarea id="commentText" placeholder="Write a comment..."></textarea>
<button id="postComment">Post</button>
<button id="signInGoogle">Sign in with Google</button>
<button id="signInGitHub">Sign in with GitHub</button>
```

---

## Optional Enhancements

- Pagination / lazy loading for large comment threads
- Comment threading: add `parent_id` to `comments`
- Reactions / likes
- Spam prevention (CAPTCHA or honeypot)
- Realtime updates using Supabase Realtime
- Minimal CSS styling or identicon avatars for anonymous users

---

## Maintenance

- Admins use Supabase dashboard for moderation
- Optional GitHub Action to ping Supabase weekly for free tier

```yaml
name: Keep Supabase Awake
on:
  schedule:
    - cron: "0 12 * * 0" # every Sunday UTC
jobs:
  ping:
    runs-on: ubuntu-latest
    steps:
      - name: Call Supabase Ping
        run: |
          curl -s https://YOUR_PROJECT.supabase.co/rest/v1/comments \
          -H "apikey: YOUR_ANON_KEY" \
          -H "Authorization: Bearer YOUR_ANON_KEY" \
          -o /dev/null
```

---

## Summary

- Fully client-side comment system for static sites
- Anonymous posting by default
- Optional Google or GitHub login
- Multiple OAuth providers can be linked to same user
- Users can edit/delete their own comments
- Admins can moderate all comments
- Supabase backend handles storage, auth, and permissions
- No Node, no NPM, no custom backend required
- Lightweight, privacy-conscious, modular, easy to embed

```

```
