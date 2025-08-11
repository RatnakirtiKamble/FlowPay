import { Navigate, useLocation } from "react-router-dom";
import { type ReactNode } from "react";
import { useAuth } from "../Context/AuthContext";

interface ProtectedRouteProps {
  children: ReactNode;
}

function ProtectedRoute({ children }: ProtectedRouteProps) {
  // Get both accessToken and loading from the context
  const { accessToken, loading } = useAuth();
  const location = useLocation();

  // 1. If we are still checking for a persisted session, show a loading indicator.
  if (loading) {
    return (
      <div className="flex justify-center items-center min-h-screen text-white text-2xl">
        Loading...
      </div>
    );
  }

  // 2. If we are done loading and there is NO accessToken, redirect to home.
  // This is the key change. We check for the token, not the user object.
  if (!accessToken) {
    // We can also pass the original location in state so the login page can redirect back here after success
    return <Navigate to="/" state={{ from: location }} replace />;
  }

  // 3. If we are done loading and there IS an accessToken, show the protected content.
  return <>{children}</>;
}

export default ProtectedRoute;