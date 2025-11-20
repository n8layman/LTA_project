/**
 * Supabase Comments System
 * A client-side comment system with anonymous posting and optional OAuth
 *
 * Usage:
 *   <script type="module">
 *     import { initComments } from './supabase-comments.js';
 *     initComments({
 *       supabaseUrl: 'YOUR_SUPABASE_URL',
 *       supabaseKey: 'YOUR_ANON_KEY'
 *     });
 *   </script>
 */

import { createClient } from "https://esm.sh/@supabase/supabase-js@2";

let supabase = null;
let currentUser = null;

/**
 * Initialize the comment system
 * @param {Object} config - Configuration object
 * @param {string} config.supabaseUrl - Supabase project URL
 * @param {string} config.supabaseKey - Supabase anonymous public key
 */
export async function initComments(config) {
  if (!config.supabaseUrl || !config.supabaseKey) {
    console.error('Supabase URL and Key are required');
    return;
  }

  // Initialize Supabase client
  supabase = createClient(config.supabaseUrl, config.supabaseKey);

  // Ensure user has a session (anonymous or authenticated)
  await ensureSession();

  // Set up the UI
  await setupCommentUI();

  // Load existing comments
  await loadComments();

  // Subscribe to real-time comment updates
  subscribeToComments();
}

/**
 * Ensure user has an active session (create anonymous if needed)
 */
async function ensureSession() {
  try {
    const { data: { session } } = await supabase.auth.getSession();

    if (!session) {
      const { data, error } = await supabase.auth.signInAnonymously();
      if (error) throw error;
      currentUser = data.user;
    } else {
      currentUser = session.user;
    }
  } catch (error) {
    console.error('Error ensuring session:', error);
  }
}

/**
 * Set up the comment UI elements
 */
async function setupCommentUI() {
  const container = document.getElementById('comments-container');
  if (!container) {
    console.warn('No #comments-container found in page');
    return;
  }

  // Get current user info
  const { data: { user } } = await supabase.auth.getUser();
  const isAnonymous = user?.is_anonymous ?? true;
  const userName = user?.user_metadata?.full_name || 'Anonymous';

  // Create comment UI structure
  container.innerHTML = `
    <div class="comments-section">
      <h3 class="comments-title">Comments</h3>

      <!-- Auth status and sign-in buttons -->
      <div class="comments-auth-status">
        ${isAnonymous ? `
          <p class="auth-message">You're posting as <strong>Anonymous</strong>.
            <button id="signin-google" class="auth-button">Sign in with Google</button> or
            <button id="signin-github" class="auth-button">Sign in with GitHub</button>
            to claim your comments.
          </p>
        ` : `
          <p class="auth-message">Signed in as <strong>${userName}</strong>.
            <button id="signout-button" class="auth-button">Sign out</button>
          </p>
        `}
      </div>

      <!-- Comment form -->
      <div class="comment-form">
        <textarea
          id="comment-input"
          class="comment-textarea"
          placeholder="Write a comment..."
          rows="4"
        ></textarea>
        <div class="comment-form-actions">
          <button id="post-comment" class="comment-submit-btn">Post Comment</button>
        </div>
      </div>

      <!-- Comments list -->
      <div id="comments-list" class="comments-list">
        <div class="loading">Loading comments...</div>
      </div>
    </div>
  `;

  // Attach event listeners
  attachEventListeners(isAnonymous);
}

/**
 * Attach event listeners to buttons
 */
function attachEventListeners(isAnonymous) {
  // Post comment
  const postBtn = document.getElementById('post-comment');
  if (postBtn) {
    postBtn.addEventListener('click', postComment);
  }

  // Handle Enter key (Ctrl/Cmd+Enter to submit)
  const textarea = document.getElementById('comment-input');
  if (textarea) {
    textarea.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && (e.ctrlKey || e.metaKey)) {
        postComment();
      }
    });
  }

  // Sign in buttons
  if (isAnonymous) {
    const googleBtn = document.getElementById('signin-google');
    const githubBtn = document.getElementById('signin-github');

    if (googleBtn) {
      googleBtn.addEventListener('click', () => signInWithProvider('google'));
    }
    if (githubBtn) {
      githubBtn.addEventListener('click', () => signInWithProvider('github'));
    }
  } else {
    const signoutBtn = document.getElementById('signout-button');
    if (signoutBtn) {
      signoutBtn.addEventListener('click', signOut);
    }
  }
}

/**
 * Sign in with OAuth provider (links to anonymous session)
 */
async function signInWithProvider(provider) {
  try {
    const { error } = await supabase.auth.linkIdentity({
      provider: provider,
    });

    if (error) throw error;
  } catch (error) {
    console.error(`Error signing in with ${provider}:`, error);
    alert(`Error signing in with ${provider}. Please try again.`);
  }
}

/**
 * Sign out current user
 */
async function signOut() {
  try {
    const { error } = await supabase.auth.signOut();
    if (error) throw error;

    // Reinitialize with new anonymous session
    await ensureSession();
    await setupCommentUI();
    await loadComments();
  } catch (error) {
    console.error('Error signing out:', error);
  }
}

/**
 * Load and display comments for current page
 */
async function loadComments() {
  const commentsList = document.getElementById('comments-list');
  if (!commentsList) return;

  try {
    const pagePath = window.location.pathname;

    const { data: comments, error } = await supabase
      .from('comments')
      .select('*')
      .eq('page_path', pagePath)
      .order('created_at', { ascending: true });

    if (error) throw error;

    if (comments.length === 0) {
      commentsList.innerHTML = '<p class="no-comments">No comments yet. Be the first to comment!</p>';
      return;
    }

    // Render comments
    commentsList.innerHTML = comments.map(comment => renderComment(comment)).join('');

    // Attach edit/delete listeners
    attachCommentActions();

  } catch (error) {
    console.error('Error loading comments:', error);
    commentsList.innerHTML = '<p class="error">Error loading comments. Please refresh the page.</p>';
  }
}

/**
 * Render a single comment as HTML
 */
function renderComment(comment) {
  const createdDate = new Date(comment.created_at).toLocaleString();
  const isEdited = comment.updated_at && comment.updated_at !== comment.created_at;
  const editedText = isEdited ? `<small class="edited-label">(edited ${new Date(comment.updated_at).toLocaleString()})</small>` : '';

  const canEdit = currentUser && comment.user_id === currentUser.id;

  const avatar = comment.avatar_url
    ? `<img src="${comment.avatar_url}" alt="${comment.name}" class="comment-avatar" />`
    : `<div class="comment-avatar-placeholder">${comment.name.charAt(0).toUpperCase()}</div>`;

  return `
    <div class="comment" data-comment-id="${comment.id}">
      <div class="comment-header">
        ${avatar}
        <div class="comment-meta">
          <strong class="comment-author">${escapeHtml(comment.name)}</strong>
          <span class="comment-date">${createdDate}</span>
          ${editedText}
        </div>
        ${canEdit ? `
          <div class="comment-actions">
            <button class="comment-action-btn edit-btn" data-id="${comment.id}">Edit</button>
            <button class="comment-action-btn delete-btn" data-id="${comment.id}">Delete</button>
          </div>
        ` : ''}
      </div>
      <div class="comment-body">
        <p class="comment-text">${escapeHtml(comment.message)}</p>
      </div>
    </div>
  `;
}

/**
 * Attach edit/delete action listeners to comment buttons
 */
function attachCommentActions() {
  // Edit buttons
  document.querySelectorAll('.edit-btn').forEach(btn => {
    btn.addEventListener('click', (e) => {
      const commentId = e.target.dataset.id;
      editComment(commentId);
    });
  });

  // Delete buttons
  document.querySelectorAll('.delete-btn').forEach(btn => {
    btn.addEventListener('click', (e) => {
      const commentId = e.target.dataset.id;
      deleteComment(commentId);
    });
  });
}

/**
 * Sanitize comment text by removing HTML tags
 * Strips all < and > characters to prevent any HTML/script injection
 */
function sanitizeComment(text) {
  if (!text) return '';
  // Remove all < and > characters
  return text.replace(/[<>]/g, '');
}

/**
 * Validate comment input
 */
function validateComment(message) {
  const MAX_COMMENT_LENGTH = 5000;

  if (!message || message.trim().length === 0) {
    return { valid: false, error: 'Please enter a comment before posting.' };
  }

  if (message.length > MAX_COMMENT_LENGTH) {
    return {
      valid: false,
      error: `Comment is too long. Maximum ${MAX_COMMENT_LENGTH} characters allowed.`
    };
  }

  return { valid: true };
}

/**
 * Post a new comment
 */
async function postComment() {
  const textarea = document.getElementById('comment-input');
  const postBtn = document.getElementById('post-comment');

  if (!textarea || !postBtn) return;

  const rawMessage = textarea.value.trim();

  // Validate input
  const validation = validateComment(rawMessage);
  if (!validation.valid) {
    alert(validation.error);
    return;
  }

  // Sanitize message - remove all HTML tags
  const message = sanitizeComment(rawMessage);

  if (!message) {
    alert('Your comment contains only invalid characters.');
    return;
  }

  // Disable form while posting
  postBtn.disabled = true;
  postBtn.textContent = 'Posting...';

  try {
    const { data: { user } } = await supabase.auth.getUser();
    const name = user?.user_metadata?.full_name || 'Anonymous';
    const avatarUrl = user?.user_metadata?.avatar_url || null;

    const { error } = await supabase
      .from('comments')
      .insert([{
        page_path: window.location.pathname,
        name: name,
        avatar_url: avatarUrl,
        message: message,
        user_id: user?.id || null
      }]);

    if (error) throw error;

    // Clear textarea
    textarea.value = '';

    // Comments will update via real-time subscription
    // But reload manually as fallback
    await loadComments();

  } catch (error) {
    console.error('Error posting comment:', error);
    alert('Error posting comment. Please try again.');
  } finally {
    postBtn.disabled = false;
    postBtn.textContent = 'Post Comment';
  }
}

/**
 * Edit an existing comment
 */
async function editComment(commentId) {
  const commentEl = document.querySelector(`[data-comment-id="${commentId}"]`);
  if (!commentEl) return;

  const textEl = commentEl.querySelector('.comment-text');
  const currentText = textEl.textContent;

  const newText = prompt('Edit your comment:', currentText);
  if (newText === null || newText.trim() === currentText.trim()) return;

  // Validate edited comment
  const validation = validateComment(newText);
  if (!validation.valid) {
    alert(validation.error);
    return;
  }

  // Sanitize edited comment
  const sanitizedText = sanitizeComment(newText.trim());
  if (!sanitizedText) {
    alert('Your comment contains only invalid characters.');
    return;
  }

  try {
    const { error } = await supabase
      .from('comments')
      .update({
        message: sanitizedText,
        updated_at: new Date().toISOString()
      })
      .eq('id', commentId);

    if (error) throw error;

    await loadComments();

  } catch (error) {
    console.error('Error editing comment:', error);
    alert('Error editing comment. Please try again.');
  }
}

/**
 * Delete a comment
 */
async function deleteComment(commentId) {
  if (!confirm('Are you sure you want to delete this comment?')) return;

  try {
    const { error } = await supabase
      .from('comments')
      .delete()
      .eq('id', commentId);

    if (error) throw error;

    await loadComments();

  } catch (error) {
    console.error('Error deleting comment:', error);
    alert('Error deleting comment. Please try again.');
  }
}

/**
 * Subscribe to real-time comment updates
 */
function subscribeToComments() {
  const pagePath = window.location.pathname;

  supabase
    .channel('comments')
    .on(
      'postgres_changes',
      {
        event: '*',
        schema: 'public',
        table: 'comments',
        filter: `page_path=eq.${pagePath}`
      },
      () => {
        loadComments();
      }
    )
    .subscribe();
}

/**
 * Escape HTML to prevent XSS
 */
function escapeHtml(text) {
  const div = document.createElement('div');
  div.textContent = text;
  return div.innerHTML;
}
