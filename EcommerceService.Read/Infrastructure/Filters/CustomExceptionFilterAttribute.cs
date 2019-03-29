using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Web;
using System.Web.Http.Filters;

namespace EcommerceService.Read.Infrastructure.Filters
{
    public class CustomExceptionFilterAttribute : ExceptionFilterAttribute
    {
        /// <summary>
        /// Called when [exception].
        /// </summary>
        /// <param name="actionContext">The action context.</param>
        public override void OnException(HttpActionExecutedContext actionContext)
        {
            string exceptionMessage = string.Empty;
            if (actionContext.Exception.InnerException == null)
            {
                exceptionMessage = actionContext.Exception.Message;
            }
            else
            {
                exceptionMessage = actionContext.Exception.InnerException.Message;
            }
            ////We can log this exception message to the file or database.  
            var errorResponse = actionContext.Request.CreateErrorResponse(HttpStatusCode.InternalServerError, actionContext.Exception);
            actionContext.Response = errorResponse;
        }
    }
}