# Comment Systems Setup Guide

This guide provides step-by-step instructions for implementing different comment systems on your Shinylive dashboard. Choose the one that best fits your needs.

---

## 📊 Quick Comparison Table

| Service | Cost | Page Loads | Anonymous | Google OAuth | Branding | Spam Filter | Moderation | Website |
|---------|------|------------|-----------|--------------|----------|-------------|------------|---------|
| **[Hyvor Talk](https://talk.hyvor.com)** Free | **$0** | 100/mo | ✅ | ✅ | Required | ✅ | ✅ | [talk.hyvor.com](https://talk.hyvor.com) |
| **[Hyvor Talk](https://talk.hyvor.com)** Paid | **$5/mo** | Unlimited | ✅ | ✅ | Optional | ✅ | ✅ | [talk.hyvor.com](https://talk.hyvor.com) |
| **[GraphComment](https://graphcomment.com)** Free | **$0** | **Unlimited** | ✅ | ✅ | Required | ✅ | ✅ | [graphcomment.com](https://graphcomment.com) |
| **[GraphComment](https://graphcomment.com)** Pro | $10/mo | Unlimited | ✅ | ✅ | None | ✅✅ | ✅✅ | [graphcomment.com](https://graphcomment.com) |
| **[Commento](https://commento.io)** | $10/mo | Unlimited | ✅ | ✅ | None | ✅✅ | ✅ | [commento.io](https://commento.io) |
| **[Remarkbox](https://remarkbox.com)** | $5/mo | Unlimited | ✅ | ✅ | None | ✅ | ✅ | [remarkbox.com](https://remarkbox.com) |
| **[Cusdis](https://cusdis.com)** Cloud | $5/mo | Unlimited | ⚠️ Approval | ❌ | None | 🟡 Basic | ✅ | [cusdis.com](https://cusdis.com) |
| **[Giscus](https://giscus.app)** (current) | $0 | Unlimited | ❌ | ❌ GitHub only | None | ✅ | ✅ GitHub | [giscus.app](https://giscus.app) |

### Legend:
- ✅ = Yes/Included
- ✅✅ = Advanced/Better
- ❌ = No/Not Available
- ⚠️ = Limited/Conditional
- 🟡 = Basic

---

## 🏆 Top Recommendations

### 🥇 Best Free Option: **[GraphComment](https://graphcomment.com)**
- **✅ Truly unlimited** page loads (no limits!)
- **✅ Google OAuth** + Anonymous
- **✅ $0 forever**
- ⚠️ Has "Powered by GraphComment" branding
- 📊 Best for: Research dashboards with variable traffic

### 🥈 Best Value Paid: **[Hyvor Talk](https://talk.hyvor.com)** ($5/mo)
- **✅ Best UI/UX** of all options
- **✅ $5/month** unlimited
- **✅ Can remove branding**
- **✅ Free tier** to test (100 page loads)
- 📊 Best for: Professional appearance on budget

### 🥉 Most Private: **[Commento](https://commento.io)** ($10/mo)
- **✅ GDPR compliant**
- **✅ No tracking/ads**
- **✅ Open source**
- **✅ Can self-host** (Docker)
- 📊 Best for: Privacy-focused projects

---

## 🔗 Service Links & Screenshots

### [Hyvor Talk](https://talk.hyvor.com)
- **Website:** https://talk.hyvor.com
- **Pricing:** https://talk.hyvor.com/pricing
- **Docs:** https://talk.hyvor.com/docs
- **Demo:** https://talk.hyvor.com/demo
- **Features:** Real-time, Reactions, Email notifications, Markdown, Rich embeds

### [GraphComment](https://graphcomment.com)
- **Website:** https://graphcomment.com
- **Pricing:** https://graphcomment.com/pricing
- **Docs:** https://graphcomment.com/documentation
- **Demo:** https://graphcomment.com/demo
- **Features:** Rich media (GIFs, images), Reactions, Threading, Email notifications

### [Commento](https://commento.io)
- **Website:** https://commento.io
- **Pricing:** https://commento.io/pricing (Flat $10/mo)
- **Docs:** https://docs.commento.io
- **Demo:** https://demo.commento.io
- **GitHub:** https://github.com/souramoo/commentoplusplus (self-host version)
- **Features:** Privacy-first, Akismet spam filter, Import from Disqus, Markdown

### [Remarkbox](https://remarkbox.com)
- **Website:** https://remarkbox.com
- **Pricing:** https://remarkbox.com/pricing (Pay-what-you-want, $5+ suggested)
- **Docs:** https://remarkbox.com/docs
- **Features:** Minimalist, Privacy-focused, Email notifications

### [Cusdis](https://cusdis.com)
- **Website:** https://cusdis.com
- **Pricing:** $5/mo cloud or free self-host
- **Docs:** https://cusdis.com/doc
- **GitHub:** https://github.com/djyde/cusdis
- **Features:** Lightweight, Open source, Email approval workflow

### [Giscus](https://giscus.app) (Your Current Setup)
- **Website:** https://giscus.app
- **Docs:** https://github.com/giscus/giscus
- **Features:** GitHub Discussions, Free, Markdown, GitHub auth only

---

## 📈 Detailed Feature Comparison

### Authentication Options

| Service | Google | GitHub | Facebook | Twitter | Anonymous | Email Required |
|---------|--------|--------|----------|---------|-----------|----------------|
| **Hyvor Talk** | ✅ | ✅ | ✅ | ✅ | ✅ | No |
| **GraphComment** | ✅ | ✅ | ✅ | ✅ | ✅ | No |
| **Commento** | ✅ | ✅ | ✅ | ✅ | ⚠️ Optional email | Optional |
| **Remarkbox** | ✅ | ✅ | ✅ | ✅ | ✅ | No |
| **Cusdis** | ❌ | ❌ | ❌ | ❌ | ⚠️ Approval flow | Yes |
| **Giscus** | ❌ | ✅ Only | ❌ | ❌ | ❌ | No |

### Moderation & Spam Features

| Service | Auto Spam Filter | Manual Moderation | Ban Users | Word Filters | Akismet |
|---------|-----------------|-------------------|-----------|--------------|---------|
| **Hyvor Talk** | ✅ | ✅ | ✅ | ✅ | ✅ (paid) |
| **GraphComment** | ✅ | ✅ | ✅ | ✅ | ✅ (pro) |
| **Commento** | ✅ | ✅ | ✅ | ❌ | ✅ |
| **Remarkbox** | ✅ | ✅ | ✅ | ✅ | Optional |
| **Cusdis** | 🟡 Basic | ✅ | ❌ | ❌ | ❌ |
| **Giscus** | ✅ GitHub | ✅ GitHub | ✅ GitHub | ❌ | ❌ |

### Display & Customization

| Service | Markdown | Rich Media | Reactions/Likes | Sorting | Nested Replies | Custom CSS |
|---------|----------|------------|-----------------|---------|----------------|------------|
| **Hyvor Talk** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **GraphComment** | ✅ | ✅✅ GIFs | ✅✅ | ✅ | ✅ | ✅ |
| **Commento** | ✅ | ❌ | ✅ Upvote | ✅ | ✅ | ✅ |
| **Remarkbox** | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ |
| **Cusdis** | 🟡 Basic | ❌ | ❌ | ❌ | ❌ | 🟡 Limited |
| **Giscus** | ✅ | ✅ | ✅✅ | ✅ | ✅ | ✅ |

---

## 💰 Pricing Breakdown

### Free Options

| Service | Monthly Cost | Page Loads | Comments | Limitations |
|---------|-------------|------------|----------|-------------|
| **GraphComment Free** | **$0** | **Unlimited** | Unlimited | Has branding, basic spam filter |
| **Hyvor Talk Free** | **$0** | **100** | Unlimited | Has branding, good spam filter |
| **Giscus** | **$0** | Unlimited | Unlimited | GitHub account required |
| **Cusdis** (self-host) | $0 | Unlimited | Unlimited | Must run Docker container |

### Paid Options

| Service | Monthly Cost | Annual Cost | Page Loads | Comments | Can Remove Branding |
|---------|-------------|-------------|------------|----------|---------------------|
| **Hyvor Talk Basic** | **$5** | $50 ($4.17/mo) | Unlimited | Unlimited | ✅ |
| **Remarkbox** | **$5+** | Varies | Unlimited | Unlimited | ✅ |
| **Cusdis Cloud** | **$5** | $60 | Unlimited | Unlimited | ✅ |
| **GraphComment Pro** | **$10** | $100 | Unlimited | Unlimited | ✅ |
| **Commento** | **$10** | $99 ($8.25/mo) | Unlimited | Unlimited | ✅ |

**💡 Best Value:** Hyvor Talk at $5/mo gives you premium features at half the cost of GraphComment Pro or Commento.

---

## 🎯 Decision Matrix

### Choose **[GraphComment Free](https://graphcomment.com)** if:
- ✅ You need **unlimited page loads**
- ✅ You want **$0 cost forever**
- ✅ You don't mind small branding footer
- ✅ You want rich media (GIFs, images, reactions)
- ✅ Your researchers will use Google accounts
- 📊 **Best for:** Research projects, academic dashboards, variable traffic

### Choose **[Hyvor Talk Free](https://talk.hyvor.com)** if:
- ✅ You have **<100 page loads/month**
- ✅ You want to **test before paying**
- ✅ You want **best UI** of free options
- ✅ You might upgrade to $5/mo later
- 📊 **Best for:** Low-traffic sites, trial period, budget uncertain

### Choose **[Hyvor Talk Paid](https://talk.hyvor.com/pricing)** ($5/mo) if:
- ✅ You need **unlimited** page loads
- ✅ You have **$5/month budget**
- ✅ You want **professional appearance**
- ✅ You want to **remove branding**
- ✅ You want **best features for the price**
- 📊 **Best for:** Professional dashboards, growing traffic, best value

### Choose **[Commento](https://commento.io/pricing)** ($10/mo) if:
- ✅ **Privacy is critical** (GDPR, no tracking)
- ✅ You want **cleanest interface**
- ✅ You want **open-source** with cloud hosting
- ✅ You have **$10/month budget**
- ✅ You might self-host later
- 📊 **Best for:** Privacy-focused projects, EU audiences, professional sites

### Keep **[Giscus](https://giscus.app)** (Free) if:
- ✅ Your researchers **all have GitHub accounts**
- ✅ You want **GitHub integration**
- ✅ **$0 budget** is critical
- ❌ You don't need anonymous posting
- 📊 **Best for:** Developer tools, open-source projects, GitHub-native audiences

---

## Option 1: Hyvor Talk

**Best for:** Low-traffic sites that want a free trial, or sites willing to pay $5/month for professional features.

### Features:
- ✅ Anonymous commenting
- ✅ Google, Facebook, Twitter, Disqus login
- ✅ Real-time comments
- ✅ Spam protection
- ✅ Moderation dashboard
- ✅ Email notifications
- ✅ Markdown support
- ✅ Mobile responsive

### Pricing:
- **Free:** 100 page loads/month (with branding)
- **Basic:** $5/month unlimited (can remove branding)
- **Pro:** $10/month (advanced features)

### Setup Instructions:

#### 1. Create Account
1. Go to https://talk.hyvor.com
2. Click "Sign Up" → Create account
3. Verify email

#### 2. Add Your Website
1. In dashboard, click "Add Website"
2. Enter:
   - **Name:** Regional Tick Surveillance Data Explorer
   - **URL:** https://n8layman.github.io/LTA_project/
3. Click "Create"
4. Copy your **Website ID** (looks like: `12345`)

#### 3. Configure Settings
1. Go to **Console** → **General**
2. Enable **Guest Commenting** (for anonymous)
3. Go to **Console** → **Login Methods**
4. Enable:
   - ✅ Guest (anonymous)
   - ✅ Google
   - ✅ Facebook (optional)
   - ✅ Twitter (optional)
5. Save changes

#### 4. Get Embed Code
Your embed code will look like this:

```html
<div id="hyvor-talk-view"></div>
<script type="text/javascript">
    var HYVOR_TALK_WEBSITE = YOUR_WEBSITE_ID; // Replace with your ID
    var HYVOR_TALK_CONFIG = {
        url: false,
        id: false
    };
</script>
<script async type="text/javascript" src="//talk.hyvor.com/web-api/embed"></script>
```

#### 5. Add to Your Dashboard

**Option A: Manual Integration**

Edit `app/export_with_giscus.R` or create a new export function:

```r
export_with_hyvor <- function(
  app_dir = "app",
  output_dir = "docs",
  hyvor_website_id = "YOUR_WEBSITE_ID"
) {

  # Export shinylive
  shinylive::export(appdir = app_dir, destdir = output_dir)

  # Read HTML
  html_file <- file.path(output_dir, "index.html")
  html_content <- readLines(html_file, warn = FALSE)

  # Create Hyvor Talk embed code
  hyvor_code <- c(
    "",
    "    <!-- Hyvor Talk Comments -->",
    "    <div style=\"max-width: 1200px; margin: 2rem auto; padding: 20px;\">",
    "      <h2 style=\"font-family: 'Inter', sans-serif; margin-bottom: 1rem;\">Comments</h2>",
    "      <div id=\"hyvor-talk-view\"></div>",
    "      <script type=\"text/javascript\">",
    sprintf("        var HYVOR_TALK_WEBSITE = %s;", hyvor_website_id),
    "        var HYVOR_TALK_CONFIG = {",
    "            url: false,",
    "            id: false",
    "        };",
    "      </script>",
    "      <script async type=\"text/javascript\" src=\"//talk.hyvor.com/web-api/embed\"></script>",
    "    </div>",
    ""
  )

  # Insert before </body>
  body_close_index <- which(grepl("</body>", html_content))
  modified_html <- c(
    html_content[1:(body_close_index - 1)],
    hyvor_code,
    html_content[body_close_index:length(html_content)]
  )

  # Update title
  modified_html <- gsub("<title>Shiny App</title>",
                        "<title>Regional Tick Surveillance Data Explorer</title>",
                        modified_html, fixed = TRUE)

  writeLines(modified_html, html_file)
  message("✓ Hyvor Talk comments added successfully")

  invisible(html_file)
}
```

**Usage:**
```r
source("app/export_with_hyvor.R")
export_with_hyvor(hyvor_website_id = "12345")  # Use your actual ID
```

**Option B: Direct HTML Edit** (Quick Test)

1. Export your app normally
2. Open `docs/index.html`
3. Find `</body>` tag
4. Paste the Hyvor embed code before it
5. Save and test locally

#### 6. Test Locally

Open `docs/index.html` in your browser. You should see:
- Hyvor Talk comment widget at bottom
- Options to sign in with Google or post as Guest
- Real-time comment preview

#### 7. Moderate Comments

1. Log into https://talk.hyvor.com
2. Go to **Moderate** tab
3. Approve/delete comments
4. Set auto-moderation rules if needed

---

## Option 2: GraphComment

**Best for:** Sites that want unlimited free commenting with Google OAuth (don't mind branding).

### Features:
- ✅ Anonymous commenting
- ✅ Google, Facebook, Twitter, Disqus login
- ✅ Unlimited page loads (free tier!)
- ✅ Real-time comments
- ✅ Spam protection
- ✅ Moderation dashboard
- ✅ Rich media (images, GIFs)
- ⚠️ Branding on free tier

### Pricing:
- **Free:** Unlimited (with "Powered by GraphComment" branding)
- **Premium:** $10/month (remove branding + advanced features)

### Setup Instructions:

#### 1. Create Account
1. Go to https://graphcomment.com
2. Click "Sign Up" → Create account
3. Verify email

#### 2. Add Your Website
1. In dashboard, click "Add a Website"
2. Enter:
   - **Website URL:** https://n8layman.github.io/LTA_project/
   - **Website Name:** Regional Tick Surveillance Data Explorer
3. Click "Add Website"
4. Copy your **GraphComment ID** (looks like: `Your-Website-Name`)

#### 3. Configure Settings
1. Go to **Settings** → **General**
2. Enable:
   - ✅ **Guest Comments** (anonymous)
   - ✅ **Require Moderation** (optional, for approval workflow)
3. Go to **Settings** → **Login**
4. Enable:
   - ✅ Guest
   - ✅ Google
   - ✅ Facebook (optional)
   - ✅ Twitter (optional)
5. Save changes

#### 4. Get Embed Code

GraphComment will provide code like this:

```html
<!-- begin graphcomment code -->
<div id="graphcomment"></div>
<script type="text/javascript">
  window.gc_params = {
    graphcomment_id: 'Your-Website-Name', // Replace with your ID
    fixed_header_height: 0,
  };

  (function() {
    var gc = document.createElement('script');
    gc.type = 'text/javascript';
    gc.async = true;
    gc.src = 'https://graphcomment.com/js/integration.js?' + Math.round(Math.random() * 1e8);
    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(gc);
  })();
</script>
<!-- end graphcomment code -->
```

#### 5. Add to Your Dashboard

**Create `app/export_with_graphcomment.R`:**

```r
export_with_graphcomment <- function(
  app_dir = "app",
  output_dir = "docs",
  graphcomment_id = "Your-Website-Name"
) {

  # Export shinylive
  shinylive::export(appdir = app_dir, destdir = output_dir)

  # Read HTML
  html_file <- file.path(output_dir, "index.html")
  html_content <- readLines(html_file, warn = FALSE)

  # Create GraphComment embed code
  gc_code <- c(
    "",
    "    <!-- GraphComment Comments -->",
    "    <div style=\"max-width: 1200px; margin: 2rem auto; padding: 20px;\">",
    "      <h2 style=\"font-family: 'Inter', sans-serif; margin-bottom: 1rem;\">Comments</h2>",
    "      <div id=\"graphcomment\"></div>",
    "      <script type=\"text/javascript\">",
    "        window.gc_params = {",
    sprintf("          graphcomment_id: '%s',", graphcomment_id),
    "          fixed_header_height: 0,",
    "        };",
    "",
    "        (function() {",
    "          var gc = document.createElement('script');",
    "          gc.type = 'text/javascript';",
    "          gc.async = true;",
    "          gc.src = 'https://graphcomment.com/js/integration.js?' + Math.round(Math.random() * 1e8);",
    "          (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(gc);",
    "        })();",
    "      </script>",
    "    </div>",
    ""
  )

  # Insert before </body>
  body_close_index <- which(grepl("</body>", html_content))
  modified_html <- c(
    html_content[1:(body_close_index - 1)],
    gc_code,
    html_content[body_close_index:length(html_content)]
  )

  # Update title
  modified_html <- gsub("<title>Shiny App</title>",
                        "<title>Regional Tick Surveillance Data Explorer</title>",
                        modified_html, fixed = TRUE)

  writeLines(modified_html, html_file)
  message("✓ GraphComment comments added successfully")

  invisible(html_file)
}
```

**Usage:**
```r
source("app/export_with_graphcomment.R")
export_with_graphcomment(graphcomment_id = "Your-Website-Name")
```

#### 6. Test Locally

Open `docs/index.html` in your browser. You should see:
- GraphComment widget at bottom
- Options to sign in with Google or comment as Guest
- Rich media support (images, reactions, etc.)

#### 7. Moderate Comments

1. Log into https://graphcomment.com
2. Go to **Moderate** tab
3. Approve/delete/reply to comments
4. Configure spam filters

---

## Option 3: Commento

**Best for:** Privacy-focused sites willing to pay $10/month for a clean, ad-free experience.

### Features:
- ✅ Anonymous commenting (optional email)
- ✅ Google, GitHub, GitLab, Twitter login
- ✅ Unlimited comments
- ✅ Privacy-focused (GDPR compliant)
- ✅ No ads, no tracking
- ✅ Spam protection with Akismet
- ✅ Import from Disqus
- ✅ Markdown support
- ✅ Email notifications

### Pricing:
- **$10/month** flat rate (unlimited sites and comments)
- Can also self-host for free (requires Docker)

### Setup Instructions:

#### 1. Create Account
1. Go to https://commento.io
2. Click "Get Started"
3. Choose **Cloud** plan ($10/month)
4. Complete signup and payment

#### 2. Add Your Website
1. In dashboard, click "Add Domain"
2. Enter: `n8layman.github.io`
3. Click "Add"
4. You'll get an embed code

#### 3. Configure Settings
1. Click on your domain
2. Go to **Settings**
3. Enable:
   - ✅ **Anonymous Comments** (or require email)
   - ✅ **Google Login**
   - ✅ **GitHub Login** (optional)
4. Set moderation preferences:
   - Auto-approve
   - OR require approval
5. Save

#### 4. Get Embed Code

```html
<div id="commento"></div>
<script defer
  src="https://cdn.commento.io/js/commento.js"
  data-css-override="https://yourdomain.com/custom-commento.css">
</script>
```

#### 5. Add to Your Dashboard

**Create `app/export_with_commento.R`:**

```r
export_with_commento <- function(
  app_dir = "app",
  output_dir = "docs"
) {

  # Export shinylive
  shinylive::export(appdir = app_dir, destdir = output_dir)

  # Read HTML
  html_file <- file.path(output_dir, "index.html")
  html_content <- readLines(html_file, warn = FALSE)

  # Create Commento embed code
  commento_code <- c(
    "",
    "    <!-- Commento Comments -->",
    "    <div style=\"max-width: 1200px; margin: 2rem auto; padding: 20px;\">",
    "      <h2 style=\"font-family: 'Inter', sans-serif; margin-bottom: 1rem;\">Comments</h2>",
    "      <div id=\"commento\"></div>",
    "      <script defer src=\"https://cdn.commento.io/js/commento.js\"></script>",
    "    </div>",
    ""
  )

  # Insert before </body>
  body_close_index <- which(grepl("</body>", html_content))
  modified_html <- c(
    html_content[1:(body_close_index - 1)],
    commento_code,
    html_content[body_close_index:length(html_content)]
  )

  # Update title
  modified_html <- gsub("<title>Shiny App</title>",
                        "<title>Regional Tick Surveillance Data Explorer</title>",
                        modified_html, fixed = TRUE)

  writeLines(modified_html, html_file)
  message("✓ Commento comments added successfully")

  invisible(html_file)
}
```

**Usage:**
```r
source("app/export_with_commento.R")
export_with_commento()
```

#### 6. Moderate Comments

1. Log into https://commento.io
2. Go to **Moderation** tab
3. Approve/delete comments
4. Reply as moderator
5. Ban users if needed

---

## Option 4: Keep Giscus (GitHub Discussions)

**Best for:** Developer-focused audiences who already have GitHub accounts.

### Current Setup:
You already have this implemented! See `app/export_with_giscus.R`

**Limitations:**
- ❌ Requires GitHub account (tick researchers may not have)
- ❌ No anonymous posting

**Keep if:**
- Your audience is technical
- You want GitHub integration
- Free is critical and you're okay with GitHub-only

---

## Recommendation Matrix

### Choose **Hyvor Talk Free** if:
- You have <100 page loads/month
- Want to try before paying
- Need Google OAuth + anonymous
- Don't mind small branding

### Choose **Hyvor Talk Paid ($5/mo)** if:
- You have >100 page loads/month
- Want professional appearance
- Budget allows $5/month
- Best features for the price

### Choose **GraphComment Free** if:
- You need unlimited page loads
- Budget is $0
- Don't mind "Powered by GraphComment" branding
- Want rich media support (GIFs, reactions)

### Choose **Commento ($10/mo)** if:
- Privacy is critical (GDPR, no tracking)
- You want cleanest, most professional option
- Budget allows $10/month
- Want open-source with cloud hosting

### Keep **Giscus (Free)** if:
- Your researchers all have GitHub accounts
- You want tight GitHub integration
- Free is critical
- You don't need anonymous posting

---

## Testing Strategy

I recommend trying them in this order:

### Week 1: **GraphComment Free**
- No risk (free, unlimited)
- Full features
- See if branding bothers you

### Week 2: **Hyvor Talk Free**
- Compare UI/features
- Check page load counts
- Decide if worth $5/month

### Week 3: **Make Decision**
- Stick with free option (GraphComment or Hyvor if under 100 loads)
- OR upgrade to Hyvor Talk $5/month
- OR splurge on Commento $10/month for premium experience

---

## Need Help?

All of these services have:
- ✅ Google OAuth (tick researchers can use Google accounts)
- ✅ Anonymous posting (no login required)
- ✅ Spam protection
- ✅ Moderation dashboards
- ✅ Simple embed codes

The main differences are:
- **Cost** (free vs $5 vs $10)
- **Page load limits** (100/month vs unlimited)
- **Branding** (present vs optional vs none)
- **UI polish** (varies)

**Can't decide?** Start with **GraphComment Free** (unlimited, no commitment) and switch later if needed. All of them are just embed codes - switching takes 5 minutes.

---

## Next Steps

1. Choose a service from above
2. Sign up and get embed code
3. Create the export function (or manually edit HTML)
4. Test locally
5. Deploy to GitHub Pages
6. Monitor usage and moderate comments

Good luck! Let me know if you need help with any step.
