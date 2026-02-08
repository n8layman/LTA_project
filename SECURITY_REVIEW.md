# Security Review: Supabase Comment System

**Date:** 2025-11-19
**Reviewer:** Claude (Automated Security Review)
**Scope:** Client-side comment system with Supabase backend

---

## Executive Summary

The Supabase comment system implementation has **several critical security issues** that must be addressed before deployment. While the basic security controls (XSS protection, RLS policies) are in place, there are vulnerabilities in authentication, data exposure, and the database schema.

**Risk Level:** 🔴 **HIGH** - Do not deploy to production without fixes

---

## Critical Issues (Must Fix)

### 1. ⚠️ Database Schema Missing `session_id` Column

**File:** [comment_system_setup_report.md](comment_system_setup_report.md#L36-L48)

**Issue:** The database schema defines a `session_id` column, but the JavaScript implementation **never populates it**.

```sql
-- Schema includes session_id
session_id uuid,
```

**Impact:**
- RLS policies reference `session_id = auth.session_id()` but this will always be NULL
- Anonymous users **cannot edit or delete their own comments** (policies fail)
- Users lose ownership when linking OAuth identity

**Fix Required:**
```javascript
// In postComment() function, add:
const { data: { session } } = await supabase.auth.getSession();

const { error } = await supabase
  .from('comments')
  .insert([{
    page_path: window.location.pathname,
    name: name,
    avatar_url: avatarUrl,
    message: message,
    user_id: user?.id || null,
    session_id: session?.id || null  // ADD THIS LINE
  }]);
```

---

### 2. ⚠️ Hardcoded Credentials in HTML

**File:** [app/export_with_supabase.R](app/export_with_supabase.R#L86-L87)

**Issue:** Supabase URL and anon key are **embedded directly in the HTML** source:

```javascript
supabaseUrl: 'https://your-project.supabase.co',
supabaseKey: 'your-anon-key'
```

**Why This Is A Problem:**
While Supabase anon keys are *designed* to be public (protected by RLS), embedding them in HTML means:
- Keys are visible in browser DevTools and page source
- Keys are committed to Git (if you commit `docs/`)
- Keys are indexed by search engines
- Keys cannot be rotated without redeploying

**Risk Level:** Medium (by design, but not best practice)

**Recommended Fix:**
Use environment variables in GitHub Actions and inject at build time:

```yaml
# .github/workflows/deploy.yml
env:
  SUPABASE_URL: ${{ secrets.SUPABASE_URL }}
  SUPABASE_ANON_KEY: ${{ secrets.SUPABASE_ANON_KEY }}
```

**Note:** The anon key is safe to expose *if and only if* your RLS policies are perfect. Any RLS mistake = full database compromise.

---

### 3. ⚠️ XSS Vulnerability in Avatar URLs

**File:** [app/supabase-comments.js](app/supabase-comments.js#L243-L245)

**Issue:** Avatar URLs from OAuth providers are inserted directly into HTML without validation:

```javascript
const avatar = comment.avatar_url
  ? `<img src="${comment.avatar_url}" alt="${comment.name}" class="comment-avatar" />`
  : `<div class="comment-avatar-placeholder">${comment.name.charAt(0).toUpperCase()}</div>`;
```

**Attack Vector:**
1. Attacker authenticates with Google/GitHub
2. If OAuth provider allows custom avatar URLs (rare but possible)
3. URL could be: `javascript:alert('XSS')` or `data:text/html,<script>alert('XSS')</script>`

**Fix Required:**
```javascript
function sanitizeAvatarUrl(url) {
  if (!url) return null;
  try {
    const parsed = new URL(url);
    // Only allow HTTPS image URLs
    if (parsed.protocol !== 'https:') return null;
    // Whitelist known OAuth providers
    const allowedDomains = ['lh3.googleusercontent.com', 'avatars.githubusercontent.com'];
    if (!allowedDomains.some(domain => parsed.hostname.endsWith(domain))) {
      return null;
    }
    return url;
  } catch {
    return null;
  }
}

const avatar = comment.avatar_url && sanitizeAvatarUrl(comment.avatar_url)
  ? `<img src="${sanitizeAvatarUrl(comment.avatar_url)}" alt="${escapeHtml(comment.name)}" class="comment-avatar" />`
  : `<div class="comment-avatar-placeholder">${escapeHtml(comment.name.charAt(0))}</div>`;
```

---

### 4. ⚠️ No Rate Limiting on Comment Submission

**File:** [app/supabase-comments.js](app/supabase-comments.js#L294-L341)

**Issue:** No client-side or database-level rate limiting on comment posting.

**Attack Scenarios:**
- Spam flooding: Automated scripts can post unlimited anonymous comments
- Database exhaustion: Fill up free tier 500 MB database
- DoS: Flood real-time subscription channels

**Fix Required:**

**Database-level (Recommended):**
```sql
-- Add rate limiting policy
create policy "rate_limit_insert" on comments
for insert with check (
  (select count(*) from comments
   where user_id = auth.uid()
   and created_at > now() - interval '1 minute') < 5
);
```

**Client-side (Basic protection):**
```javascript
let lastCommentTime = 0;
const RATE_LIMIT_MS = 10000; // 10 seconds between comments

async function postComment() {
  const now = Date.now();
  if (now - lastCommentTime < RATE_LIMIT_MS) {
    alert('Please wait before posting another comment.');
    return;
  }
  lastCommentTime = now;
  // ... rest of function
}
```

---

## High Priority Issues (Should Fix)

### 5. 🟡 Missing Input Validation on Comment Length

**File:** [app/supabase-comments.js](app/supabase-comments.js#L300-L304)

**Issue:** No maximum length validation on comments.

**Impact:**
- Users can post arbitrarily long comments (megabytes of text)
- Database bloat
- UI rendering issues

**Fix Required:**
```javascript
const MAX_COMMENT_LENGTH = 5000;

const message = textarea.value.trim();
if (!message) {
  alert('Please enter a comment before posting.');
  return;
}
if (message.length > MAX_COMMENT_LENGTH) {
  alert(`Comment is too long. Maximum ${MAX_COMMENT_LENGTH} characters.`);
  return;
}
```

**Database constraint:**
```sql
alter table comments add constraint message_length_check
  check (char_length(message) <= 5000);
```

---

### 6. 🟡 Insufficient Admin Protection

**File:** [comment_system_setup_report.md](comment_system_setup_report.md#L54-L56)

**Issue:** Admin table lacks any security controls.

**Current Schema:**
```sql
create table admins (
  id uuid primary key references auth.users(id) on delete cascade
);
```

**Problems:**
- No RLS policies on admins table
- Anyone with database access can add themselves as admin
- No audit trail for admin additions

**Fix Required:**
```sql
-- Enable RLS on admins table
alter table admins enable row level security;

-- Only existing admins can read admin list
create policy "admins_read" on admins
for select to authenticated using (
  exists (select 1 from admins where id = auth.uid())
);

-- Prevent all INSERT/UPDATE/DELETE via policy
-- (must be done via SQL console by superuser)
create policy "admins_no_modify" on admins
for all using (false);
```

---

### 7. 🟡 Real-time Subscription Allows Data Leakage

**File:** [app/supabase-comments.js](app/supabase-comments.js#L400-L417)

**Issue:** Real-time subscription uses wildcard filter that may expose data from other pages.

```javascript
filter: `page_path=eq.${pagePath}`
```

**Problem:** If `pagePath` contains special characters or is manipulated, the filter could break.

**Fix Required:**
```javascript
function subscribeToComments() {
  const pagePath = window.location.pathname;

  // Validate pagePath to prevent filter injection
  if (typeof pagePath !== 'string' || pagePath.length > 500) {
    console.error('Invalid page path for subscription');
    return;
  }

  supabase
    .channel(`comments:${pagePath}`) // Unique channel per page
    .on(
      'postgres_changes',
      {
        event: '*',
        schema: 'public',
        table: 'comments',
        filter: `page_path=eq.${pagePath.replace(/[^a-zA-Z0-9/_-]/g, '')}` // Sanitize
      },
      () => {
        loadComments();
      }
    )
    .subscribe();
}
```

---

## Medium Priority Issues (Nice to Have)

### 8. 🟢 No CSRF Protection

**Issue:** Comment system doesn't implement CSRF tokens.

**Mitigation:** Supabase authentication handles this through JWT tokens in the session. Not critical for anonymous posting, but authenticated actions should verify origin.

**Recommendation:** Add `Referer` or `Origin` header validation in RLS policies if possible.

---

### 9. 🟢 Prompt() Dialog for Editing is Poor UX and Insecure

**File:** [app/supabase-comments.js](app/supabase-comments.js#L353)

**Issue:** Uses browser `prompt()` for editing, which:
- Cannot sanitize input before display
- Shows raw unescaped text
- Poor user experience

**Fix:** Use a proper textarea modal instead of `prompt()`.

---

### 10. 🟢 Missing Content Security Policy (CSP)

**Issue:** No CSP headers to prevent XSS.

**Recommendation:** Add CSP meta tag to HTML:
```html
<meta http-equiv="Content-Security-Policy"
      content="default-src 'self';
               script-src 'self' https://esm.sh;
               connect-src 'self' https://your-project.supabase.co;
               img-src 'self' https: data:;
               style-src 'self' 'unsafe-inline';">
```

---

## Security Best Practices Review

### ✅ What's Done Well

1. **XSS Protection:** `escapeHtml()` function properly escapes user content
2. **RLS Enabled:** Row Level Security is enabled on comments table
3. **Parameterized Queries:** Supabase client uses parameterized queries (SQL injection protected)
4. **HTTPS Only:** Using Supabase's HTTPS endpoints
5. **OAuth Integration:** Proper use of `linkIdentity()` for anonymous→authenticated flow
6. **No Eval:** No use of `eval()` or `Function()` constructors

### ❌ What's Missing

1. **Input validation** (length, format)
2. **Rate limiting** (spam protection)
3. **Admin security** (RLS on admins table)
4. **Avatar URL validation** (XSS vector)
5. **session_id tracking** (broken ownership for anonymous users)
6. **Content Security Policy**
7. **Audit logging** (who modified what, when)

---

## RLS Policy Review

### Current Policies from Setup Report

```sql
-- ✅ GOOD: Public read access
create policy "public_read" on comments for select using (true);

-- ✅ GOOD: Public insert (but needs rate limiting)
create policy "public_insert" on comments for insert with check (true);

-- ⚠️ BROKEN: session_id check fails (column never populated)
create policy "owner_update" on comments for update using (
  auth.uid() = user_id or session_id = auth.session_id()
);

-- ⚠️ BROKEN: session_id check fails (column never populated)
create policy "owner_delete" on comments for delete using (
  auth.uid() = user_id or session_id = auth.session_id()
);

-- ✅ GOOD: Admin policies
create policy "admin_update" on comments for update to authenticated using (
  exists (select 1 from admins where id = auth.uid())
);
```

**Critical Fix:**
The `session_id` must be populated in the INSERT, or the policies should be updated to not rely on it.

---

## Recommendations Summary

### 🔴 Critical (Fix Before Deployment)

1. **Add `session_id` to comment inserts** (or remove from RLS policies)
2. **Validate and sanitize avatar URLs** (prevent XSS)
3. **Implement rate limiting** (prevent spam/DoS)

### 🟡 High Priority (Fix Soon)

4. **Add comment length validation** (client + database)
5. **Secure admins table with RLS** (prevent privilege escalation)
6. **Sanitize subscription filters** (prevent injection)

### 🟢 Medium Priority (Improvements)

7. **Replace `prompt()` with modal UI** (better UX and security)
8. **Add Content Security Policy** (defense in depth)
9. **Use GitHub Secrets** for credentials (don't commit to Git)
10. **Add audit logging** (track modifications)

---

## Testing Recommendations

Before deploying:

1. **Test Anonymous Flow:**
   - Post comment anonymously
   - Verify you can edit/delete your own comment
   - Link to Google/GitHub
   - Verify comments persist after linking

2. **Test Attack Scenarios:**
   - Try XSS in comment text: `<script>alert('XSS')</script>`
   - Try XSS in name: `<img src=x onerror=alert('XSS')>`
   - Try SQL injection: `'; DROP TABLE comments; --`
   - Try posting 10,000 comments rapidly
   - Try posting 1 MB comment

3. **Test RLS Policies:**
   - User A posts comment
   - User B tries to edit User A's comment (should fail)
   - Admin edits User A's comment (should succeed)
   - Anonymous user tries to edit their comment after browser refresh (should work if session_id is tracked)

---

## Conclusion

The implementation has a solid foundation but requires **critical security fixes** before production use. The most important issues are:

1. Broken ownership tracking for anonymous users (session_id)
2. Missing rate limiting (spam vulnerability)
3. Avatar URL XSS risk

With these fixes applied, the system will provide reasonable security for a public comment system. However, given that comments are anonymous by default, expect moderation overhead and consider implementing:

- Shadowbanning for repeat offenders
- Keyword filters for spam
- Admin dashboard for bulk moderation

**Recommendation:** Fix critical issues, test thoroughly, then deploy to production. Consider a soft launch with moderation enabled.
